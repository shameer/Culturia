;; guile-wiredtiger - 0.1 - 2015/08/06

;; Copyright © 2014-2015 Amirouche BOUBEKKI <amirouche@hypermove.net>

;; guile-wiredtiger is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.

;; guile-wiredtiger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with guile-wiredtiger.  If not, see <http://www.gnu.org/licenses/>
(define-module (wiredtiger))

(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-field & set-fields
(use-modules (rnrs bytevectors))

(use-modules (ice-9 iconv))  ;; string->bytevector
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 receive))

(use-modules (system foreign))  ;; ffi

;;;
;;; Packing
;;;
;;
;; Adapted from wiredtiger Python bindings
;;

;; packing helpers

(define (number->byte-list value)
  ;; this is the *inversed* 64 bit representation of value
  (list
   (logand 255 9000)
   (logand 255 (ash 9000 -8))
   (logand 255 (ash 9000 -16))
   (logand 255 (ash 9000 -24))
   (logand 255 (ash 9000 -32))
   (logand 255 (ash 9000 -40))
   (logand 255 (ash 9000 -48))
   (logand 255 (ash 9000 -56))))

(define (string->byte-list string)
  (bytevector->u8-list (string->bytevector string "utf-8")))

(define (find-end-of-mark bv mark i)
  (if (= (bytevector-u8-ref bv i) mark)
      (find-end-of-mark bv mark (+ i 1))
      i))

(define (bytevector-copy source dest from-index at-index)
  (if (= from-index (bytevector-length source))
      dest
      (begin
        (bytevector-u8-set! dest at-index (bytevector-u8-ref source from-index))
        (bytevector-copy source dest (+ from-index 1) (+ at-index 1)))))

(define (bytevector-take bv index)
  (letrec ((%take% (lambda (current out)
                     (if (= current index)
                         out
                         (begin
                           (bytevector-u8-set! out current (bytevector-u8-ref bv current))
                           (%take% (+ current 1) out))))))
    (%take% 0 (make-bytevector index))))

(define-public (bytevector-drop bv index)
  (bytevector-copy bv (make-bytevector (- (bytevector-length bv) index)) index 0))

(define (long-long-bytevector x)
  (let ((out (make-bytevector 8)))
    (bytevector-u64-set! out 0 x (endianness big))
    out))

(define* (bytevector-find bv v #:optional (offset 0))
  (if (equal? (bytevector-u8-ref bv 0) v)
      offset
      (bytevector-find (bytevector-drop bv 1) v (+ offset 1))))

(define (bytevector-append bv others)
  (if (null? others)
      bv
      (letrec* ((other (car others))
                (out (make-bytevector (+ (bytevector-length bv) (bytevector-length other)))))
        (bytevector-copy! bv 0 out 0 (bytevector-length bv))
        (bytevector-copy! other 0 out (bytevector-length bv) (bytevector-length other))
        (bytevector-append out (cdr others)))))

(define (char-in c seq)
  (if (= (string-length seq) 0)
      #f
      (if (equal? c (string-ref seq 0))
          #t
          (char-in c (string-drop seq 1)))))

(define (one-if-zero x)
  (if (eq? x 0) 1 x))

;;; integer packing & unpacking

;; Variable-length integer packing
;; need: up to 64 bits, both signed and unsigned

;; Try hard for small values (up to ~2 bytes), after that, just encode the
;; length in the first byte.

;;  First byte | Next |                        |
;;  byte       | bytes| Min Value              | Max Value
;; ------------+------+------------------------+--------------------------------
;; [00 00xxxx] | free | N/A                    | N/A
;; [00 01llll] | 8-l  | -2^64                  | -2^13 - 2^6
;; [00 1xxxxx] | 1    | -2^13 - 2^6            | -2^6 - 1
;; [01 xxxxxx] | 0    | -2^6                   | -1
;; [10 xxxxxx] | 0    | 0                      | 2^6 - 1
;; [11 0xxxxx] | 1    | 2^6                    | 2^13 + 2^6 - 1
;; [11 10llll] | l    | 2^14 + 2^7             | 2^64 - 1
;; [11 11xxxx] | free | N/A                    | N/A

(define neg-multi-marker #x10)
(define neg-2byte-marker #x20)
(define neg-1byte-marker #x40)
(define pos-1byte-marker #x80)
(define pos-2byte-marker #xc0)
(define pos-multi-marker #xe0)

(define neg-1byte-min (* -1 (integer-expt -2 6)))
(define neg-2byte-min (+ (integer-expt -2 13) neg-1byte-min))
(define pos-1byte-max (- (integer-expt 2 6) 1))
(define pos-2byte-max (+ (integer-expt 2 13) pos-1byte-max))

(define minus-bit (ash -1 64))
(define uint64-mask #xffffffffffffffff)

(define* (get-bits x start #:optional (end 0))
  (ash (logand x (- (ash 1 start) 1)) (* -1 end)))

(define* (get-int bytes #:optional (value 0))
  (if (null? bytes) value
      (get-int (cdr bytes) (logior (ash value 8) (car bytes)))))

(define-public (pack-integer x)
  (cond ((< x neg-2byte-min)
         (letrec* ((bytes (number->byte-list (logand x uint64-mask)))
                   (length (list-index bytes 255))
                   (tail (reverse (list-head bytes length)))
                   (head (logior neg-multi-marker (get-bits (- 8 length) 4))))
           (cons head tail)))
        ((< x neg-1byte-min)
         (let ((x2 (- x neg-2byte-min)))
           (list (logior neg-2byte-marker (get-bits x2 13 8)) (get-bits x2  8))))
        ((< x 0) (let ((x2 (- x neg-1byte-min)))
                   (list (logior neg-1byte-marker (get-bits x 6)))))
        ((<= x pos-1byte-max) (list (logior pos-1byte-marker (get-bits x 6))))
        ((<= x pos-2byte-max) (let ((x2 (- x (+ pos-2byte-max 1))))
                              (list (logior pos-2byte-marker (get-bits x2 13 8)) (get-bits x2 8))))
        (else  (letrec* ((bytes (number->byte-list (- x (+ 1 pos-2byte-max))))
                         (length (list-index bytes 0))
                         (tail (reverse (list-head bytes length)))
                         (head (logior pos-multi-marker (get-bits length 4))))
                 (cons head tail)))))

(define-public (unpack-integer bytes)
  (let ((marker (car bytes)))
    (cond ((< marker neg-2byte-marker)
           (let ((sz (- 8 (get-bits marker 4))))
             (values (logior (ash -1 (ash sz 3)) (get-int (list-head (list-tail bytes 1) sz))
                      (list-tail bytes (+ sz 1))))))
          ((< marker neg-1byte-marker)
           (values (+ neg-2byte-min (logior (ash (get-bits marker 5) 8) (cadr bytes)))
                   (list-tail bytes 2)))
          ((< marker pos-1byte-marker)
           (values (+ neg-1byte-min (get-bits marker 6)) (list-tail bytes 1)))
          ((< marker pos-2byte-marker)  (values (get-bits marker 6) (list-tail bytes 1)))
          ((< marker pos-multi-marker)
           (values (+ pos-1byte-max 1 (logior (ash (get-bits marker 5) 8)) (cadr bytes))
                   (list-tail bytes 2)))
          (else (let ((sz (get-bits marker 4)))
                  (values (+ pos-2byte-max 1 (get-int (list-head (list-tail bytes 1) sz)))
                          (list-tail bytes (+ sz 1))))))))

;; pack and unpack implementation

(define (get-type fmt)
 (let ((tfmt (string-ref fmt 0)))
  (if (char-in tfmt ".@<>")
   (values tfmt (string-drop fmt 1))
   (values "." fmt))))

(define (parse-format fmt)
  (if (string->number (string-take fmt 1))
      (values (string-drop fmt 2) (string->number (string-take fmt 1)) (string-ref fmt 1))
      (values (string-drop fmt 1) 0 (string-ref fmt 0))))

(define (unpack-integers bytes number out)
  (if (equal? number 0)
      (values bytes out)
      (receive (value bytes) (unpack-integer bytes)
        (unpack-integers bytes (- number 1) (cons value out)))))

(define (unpack-rec fmt bytes out)
  (if (= (string-length fmt) 0)
      out
      (receive (fmt size char) (parse-format fmt)
        (cond
         ;; variable length string
         ((equal? char #\S)
          (letrec* ((end (list-index bytes 0))
                    (tail (list-tail bytes (+ end 1)))
                    (head (list-head bytes end))
                    (string (bytevector->string (list->u8vector head) "utf8")))
            (unpack-rec fmt tail (cons string out))))
         ;; variable length bytevector
         ((or (equal? char #\u) (equal? char #\U))
          (receive (size bytes) (unpack-integer bytes)
            (letrec* ((tail (list-tail bytes size))
                      (head (list-head bytes size)))
              (if (equal? char #\u)
                  (unpack-rec fmt tail (cons (list->u8vector head) out))
                  (unpack-rec fmt tail (cons (car (unpack "u" head)) out))))))
         (else ;; integral type
          (receive (bytes out) (unpack-integers bytes (one-if-zero size) out)
            (unpack-rec fmt bytes out)))))))

(define-public (unpack fmt bytes)
  (if (bytevector? bytes)
      (reverse (unpack-rec fmt (bytevector->u8-list bytes) '()))
      (reverse (unpack-rec fmt bytes '()))))

(define (make-next fmt vs)
  (letrec* ((size (string->number (string-take fmt 1)))
            (char (string-ref fmt (if size 1 0)))
            (out (string-drop fmt (if size 2 1))))
    (values out (cdr vs) char (if size size 0) (car vs))))

(define (pack-integers-rec size vs out)
  (if (= size 0)
      (values out vs)
      (let ((integer (pack-integer (car vs))))
        (pack-integers-rec (- size 1) (cdr vs) (append out integer)))))

(define (pack-rec fmt vs out)
  (if (= (string-length fmt) 0)
      out
      (receive (fmt vs char size value) (make-next fmt vs)
        (cond
         ;; variable length string
         ((equal? char #\S)
          (pack-rec fmt vs (append out (string->byte-list value) '(0))))

         ;; variable length bytevector
         ((or (equal? char #\u) (equal? char #\U))
          (pack-rec fmt vs (append out (pack-integer (bytevector-length value)) (bytevector->u8-list value))))

         ;; integral type
         (else
          (receive (out vs) (pack-integers-rec (one-if-zero size) (cons value vs) out)
            (pack-rec fmt vs out)))))))

(define-public (pack fmt . vs)
  (u8-list->bytevector (pack-rec fmt vs '())))

;; helper to store arbitrary values
;; in wiredtiger and take advantage
;; of ordering for strings and integers

(define-public (scm->bytevector scm)
  (define (scm->string scm)
    (with-output-to-string
      (lambda ()
        (write scm))))

  (cond
   ((exact-integer? scm) (pack "QQ" 0 scm))
   ((string? scm) (pack "QS" 1 scm))
   (else (pack "QS" 2 (scm->string scm)))))

(define-public (bytevector->scm bv)
  (define (string->scm value)
    (with-input-from-string value
      (lambda ()
        (read))))

  (receive (kind value) (unpack-integer (bytevector->u8-list bv))
    (cond
     ((eq? kind 0) (unpack "Q" value))
     ((eq? kind 1) (unpack "S" value))
     (else (string->scm (car (unpack "S" value)))))))

;;;
;;; Guile helpers
;;;
;;
;; macro to quickly define immutable records
;;
;; FIXME: Taken from Guile (maybe should be in (srfi srfi-99))
;;        adapted to make it possible to declare record type like `<abc>' and keep
;;        field accessor bracket free. record name *must* have brackets or everything
;;        is broken
;;
;; Usage:
;;
;;   (define-record-type <abc> field-one field-two)
;;   (define zzz (make-abc 1 2))
;;   (abc-field-one zzz) ;; => 1
;;
;; FIXME: maybe this is less useful than the immutable record of (srfi srfi-9 gnu)
;;        I still use `set-field` and `set-fields`
;;
(define-syntax define-record-type*
  (lambda (x)
    (define (%id-name name) (string->symbol (string-drop (string-drop-right (symbol->string name) 1) 1)))
    (define (id-name ctx name)
      (datum->syntax ctx (%id-name (syntax->datum name))))
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ rname field ...)
       (and (identifier? #'rname) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'rname #'make- (id-name #'rname #'rname)))
                     (pred (id-append #'rname (id-name #'rname #'rname) #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f (id-name #'rname #'rname) #'- f))
                                        #'(field ...))))
         #'(define-record-type rname
             (cons field ...)
             pred
             (field getter)
             ...))))))

;;; ffi helpers

(define *NULL* %null-pointer)
(define *pointer* '*)

;; This is small syntax change
(define* ((dynamic-link* shared-object) func-name)
  (dynamic-func func-name shared-object))

;;; foreign macro

(define-syntax-rule (foreign
                     ;; function pointer and signature
                     (ret function-pointer args ...)
                     ;; foreign-function lambda wrapper
                     wrapper)
  (let ((foreign-function (pointer->procedure ret
                                              function-pointer
                                              (list args ...))))
    (lambda  rest
        (apply wrapper (append (list foreign-function) rest)))))

;;: utils

(define (make constructor constructor-structure pointer size)
  "Convert a POINTER to a structure of SIZE into a record
   using CONSTRUCTOR and CONSTRUCTOR-STRUCTURE"
  (let* ((pointer (make-pointer (array-ref pointer 0)))
         (array (pointer->bytevector pointer size 0 'u64))
         (structure (apply constructor-structure (map make-pointer (array->list array)))))
    (constructor pointer structure)))

;;;
;;; wiredtiger bindings
;;;

(define wiredtiger (dynamic-link "libwiredtiger.so"))
(define wiredtiger* (dynamic-link* wiredtiger))


(define	WT_NOTFOUND -31803)

;;
;; (wiredtiger-error-string code)
;;

(define* (%wiredtiger-string-error call)
  (foreign
   (*pointer* (wiredtiger* "wiredtiger_strerror") int)
   (lambda (foreign-function code)
     (let ((message (pointer->string (foreign-function code))))
       (format #t "wiredtiger error while calling ~a: ~a" call message))
       ;; here we use (exit) instead of (error) which outputs a not very useful traceback
       (exit -1))))

(define (wiredtiger-string-error call message)
  ((%wiredtiger-string-error call) message))


;;;
;;; Connection
;;;

(define-record-type* <connection> handle structure)

(set-record-type-printer! <connection>
                          (lambda (record port)
                            (format port
                                    "<session 0x~x>"
                                    (pointer-address (connection-handle record)))))

;; record holding structure pointers
(define-record-type* <connection-structure>
  async-flush
  async-new-op
  close
  reconfigure
  get-home
  configure-method
  is-new
  open-session
  load-extension
  add-data-source
  add-collator
  add-compressor
  add-encryptor
  add-extractor
  get-extension-api)

(define-public connection-open
  (foreign
   (int (wiredtiger* "wiredtiger_open") *pointer* *pointer* *pointer* *pointer*)
   (lambda (foreign-function home config)
     (let* (;; init a double pointer
            (pointer #u64(0))
            (double-pointer (bytevector->pointer pointer))
            ;; convert arguments to c types
            (%home (string->pointer home))
            (%config (string->pointer config))
            ;; call the foreign function
            ;; FIXME: add support for error_handler
           (code (foreign-function %home *NULL* %config double-pointer)))
       (if (eq? code 0)
           (make make-connection make-connection-structure pointer 15)
           (let ((message (format #false "(wiredtiger-open ~s ~s)" home config)))
             (wiredtiger-string-error message code)))))))

(define (%connection-close connection)
  (foreign
   (int  (connection-structure-close (connection-structure connection)) *pointer* *pointer*)
   (lambda (foreign-function config)
     (let* (;; init a double pointer
            (pointer #u64(0))
            (double-pointer (bytevector->pointer pointer))
            ;; convert arguments to c types
            (%config (string->pointer config))
            ;; call the foreign function
            ;; FIXME: add support for error_handler
            (code (foreign-function (connection-handle connection) %config)))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(connection-close ~s ~s)" connection config)))
             (wiredtiger-string-error message code)))))))

(define*-public (connection-close connection #:optional (config ""))
  ((%connection-close connection) config))

;;;
;;; Session
;;;

(define-record-type* <session> handle structure)

(set-record-type-printer! <session>
                          (lambda (record port)
                            (format port
                                    "<session 0x~x>"
                                    (pointer-address (session-handle record)))))

;; record holding structure pointers
(define-record-type* <session-structure>
  connection
  %app-private%
  close
  reconfigure
  string-error
  cursor-open
  create
  compact
  drop
  log-printf
  rename
  salvage
  truncate
  upgrade
  verify
  transaction-begin
  transaction-commit
  transaction-rollback
  checkpoint
  snapshot
  transaction-pinned-range
  transaction-sync)

(define (%session-string-error session)
  (foreign
   (int (session-structure-string-error (session-structure session)) *pointer* int)
   (lambda (foreign-function code)
     (format #true
             "wiredtiger session error: ~a"
             (pointer->string (make-pointer (foreign-function (session-handle session) code))))
     (exit -1))))

(define-public (session-string-error session code)
  ((%session-string-error session) code))

(define (%session-open connection)
  (foreign
   (int  (connection-structure-open-session (connection-structure connection)) *pointer* *pointer* *pointer* *pointer*)
   (lambda (foreign-function config)
     (let* (;; init a double pointer
            (pointer #u64(0))
            (double-pointer (bytevector->pointer pointer))
            ;; convert arguments to c types
            (%config (string->pointer config))
            ;; call the foreign function
            ;; FIXME: add support for error_handler
            (code (foreign-function (connection-handle connection) *NULL* %config double-pointer)))
       (if (eq? code 0)
           (make make-session make-session-structure pointer 22)
           (let ((message (format #false "(session-open ~s ~s)" connection config)))
             (wiredtiger-string-error message code)))))))

(define*-public (session-open connection #:optional (config ""))
  ((%session-open connection) config))

(define (%session-create session)
  (foreign
   (int  (session-structure-create (session-structure session)) *pointer* *pointer* *pointer*)
   (lambda (foreign-function name config)
     (let* (;; convert arguments to c types
            (%name (string->pointer name))
            (%config (string->pointer config))
            ;; call the foreign function
            (code (foreign-function (session-handle session) %name %config)))
       (if (not (eq? code 0))
           (let ((message (format #false "(session-create ~s ~s)" name config)))
             (wiredtiger-string-error message code)))))))

(define-public (session-create session name config)
  ((%session-create session) name config))

(define (%session-close session)
  (foreign
   (int  (session-structure-close (session-structure session)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (session-handle session))))
       (if (not (eq? code 0))
           (let ((message (format #false "(session-close ~s)")))
             (wiredtiger-string-error message code)))))))

(define-public (session-close session)
  ((%session-close session)))

(define (%session-transaction-begin session)
  (foreign
   (int (session-structure-transaction-begin (session-structure session)) *pointer* *pointer*)
   (lambda (foreign-function config)
     (let* ((%config (string->pointer config))
            ;; call the foreign function
            (code (foreign-function (session-handle session) %config)))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(session-transaction-begin ~s ~s)" session config)))
             (wiredtiger-string-error message code)))))))

(define*-public (session-transaction-begin session #:optional (config ""))
  ((%session-transaction-begin session) config))

(define (%session-transaction-commit session)
  (foreign
   (int  (session-structure-transaction-commit (session-structure session)) *pointer* *pointer*)
   (lambda (foreign-function config)
     (let* ((%config (string->pointer config))
            ;; call the foreign function
            (code (foreign-function (session-handle session) %config)))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(session-transaction-commit ~s ~s)" session config)))
             (wiredtiger-string-error message code)))))))

(define*-public (session-transaction-commit session #:optional (config ""))
  ((%session-transaction-commit session) config))

(define (%session-transaction-rollback session config)
  (foreign
   (int  (session-structure-transaction-rollback (session-structure session)) *pointer* *pointer*)
   (lambda (foreign-function name config)
     (let* ((%config (string->pointer config))
            ;; call the foreign function
            (code (foreign-function (session-handle session) %config)))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(session-transaction-rollback ~s)")))
             (wiredtiger-string-error message code)))))))

(define*-public (session-transaction-rollback session #:optional (config ""))
  ((%session-transaction-rollback session) config))

;;;
;;; Item
;;;

(define-record-type* <item> handle bv)

(set-record-type-printer! <item>
                          (lambda (record port)
                            (format port
                                    "<item 0x~x>"
                                    (pointer-address (item-handle record)))))


;; record holding structure pointers
(define-record-type* <item-structure>
  data
  size
  ;; internal fields
  flags
  mem
  mem-size
)

;;;
;;; Cursor
;;;

(define-record-type* <cursor> handle structure)

(set-record-type-printer! <cursor>
                          (lambda (record port)
                            (format port
                                    "<cursor 0x~x>"
                                    (pointer-address (cursor-handle record)))))

;; record holding structure pointers
(define-record-type* <cursor-structure>
  cursor
  uri
  key-format
  value-format
  key-ref
  value-ref
  key-set
  value-set
  compare
  equals
  next
  previous
  reset
  search
  search-near
  insert
  update
  remove
  close
  reconfigure
  ;; XXX: other fields are defined in the header
  ;;      those are only useful to implement a new cursor type
  ;;      and as such are not part the record
)

(define (cursor-key-format cursor)
  (pointer->string (cursor-structure-key-format (cursor-structure cursor))))

(define (cursor-value-format cursor)
  (pointer->string (cursor-structure-value-format (cursor-structure cursor))))

(define (%cursor-open session)
  (foreign
   (int (session-structure-cursor-open (session-structure session)) *pointer* *pointer* *pointer* *pointer* *pointer*)
   (lambda (foreign-function uri config)
     (let* (;; init a double pointer
            (pointer #u64(0))
            (double-pointer (bytevector->pointer pointer))
            ;; convert arguments to c types
            (%uri (string->pointer uri))
            (%config (string->pointer (string-append "raw," config)))
            ;; call the foreign function
            (code (foreign-function (session-handle session) %uri *NULL* %config double-pointer)))
       (if (eq? code 0)
           (make make-cursor make-cursor-structure pointer 20)
           (let ((message (format #false "(cursor-open ~s ~s)" uri config)))
             (wiredtiger-string-error message code)))))))

(define*-public (cursor-open session uri #:optional (config ""))
  ((%cursor-open session) uri config))

(define (%cursor-key-ref cursor)
  (foreign
   (int (cursor-structure-key-ref (cursor-structure cursor)) *pointer* *pointer*)
   (lambda (foreign-function)
     (let* (;; init empty item structure
            (item #u64(0 0 0 0 0))
            (pointer (bytevector->pointer item))
            ;; call the foreign function
            (code (foreign-function (cursor-handle cursor) pointer)))
       (if (eq? code 0)
           (let ((bv (pointer->bytevector (make-pointer (array-ref item 0))
                                          (array-ref item 1)
                                          0
                                          'u64)))
             (unpack (cursor-key-format cursor) bv))
           (let ((message (format #false "(cursor-key-ref ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-key-ref cursor)
  ((%cursor-key-ref cursor)))

(define (%cursor-value-ref cursor)
  (foreign
   (int (cursor-structure-value-ref (cursor-structure cursor)) *pointer* *pointer*)
   (lambda (foreign-function)
     (let* (;; init empty item structure
            (item #u64(0 0 0 0 0))
            (pointer (bytevector->pointer item))
            ;; call the foreign function
            (code (foreign-function (cursor-handle cursor) pointer)))
       (if (eq? code 0)
           (let ((bv (pointer->bytevector (make-pointer (array-ref item 0))
                                          (array-ref item 1)
                                          0
                                          'u64)))
             (unpack (cursor-value-format cursor) bv))
           (let ((message (format #false "(cursor-value-ref ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-value-ref cursor)
  ((%cursor-value-ref cursor)))

(define (%cursor-key-set cursor)
  (foreign
   (int (cursor-structure-key-set (cursor-structure cursor)) *pointer* *pointer*)
   (lambda (foreign-function key)
     (let* (;; init item structure
            (bv (apply pack (append (list (cursor-key-format cursor)) key)))
            (bv* (bytevector->pointer bv))
            (address (pointer-address bv*))
            (size (bytevector-length bv))
            (item (list->u64vector (list address size 0 0 0)))
            (pointer (bytevector->pointer item)))
       ;; call the foreign function
       (foreign-function (cursor-handle cursor) pointer)))))

(define-public (cursor-key-set cursor . key)
  ((%cursor-key-set cursor) key))

(define (%cursor-value-set cursor)
  (foreign
   (int (cursor-structure-value-set (cursor-structure cursor)) *pointer* *pointer*)
   (lambda (foreign-function value)
     (let* (;; init item structure
            (bv (apply pack (append (list (cursor-value-format cursor)) value)))
            (bv* (bytevector->pointer bv))
            (address (pointer-address bv*))
            (size (bytevector-length bv))
            (item (list->u64vector (list address size 0 0 0)))
            (pointer (bytevector->pointer item)))
       ;; call the foreign function
       (foreign-function (cursor-handle cursor) pointer)))))

(define-public (cursor-value-set cursor . value)
  ((%cursor-value-set cursor) value))

(define (%cursor-reset cursor)
  (foreign
   (int (cursor-structure-reset (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(cursor-reset ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-reset cursor)
  ((%cursor-reset cursor)))

(define (%cursor-next cursor)
  (foreign
   (int (cursor-structure-next (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (if (eq? code WT_NOTFOUND)
               #false
               (let ((message (format #false "(cursor-next ~a)" cursor)))
                 (wiredtiger-string-error message code))))))))

(define-public (cursor-next cursor)
  ((%cursor-next cursor)))

(define (%cursor-previous cursor)
  (foreign
   (int (cursor-structure-previous (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(cursor-previous ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-previous cursor)
  ((%cursor-previous cursor)))

(define (%cursor-search cursor)
  (foreign
   (int (cursor-structure-search (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           #false)))))

(define-public (cursor-search cursor)
  ((%cursor-search cursor)))

(define (%cursor-search-near cursor)
  (foreign
   (int (cursor-structure-search-near (cursor-structure cursor)) *pointer* *pointer*)
   (lambda (foreign-function)
     (let* (;; init a integer pointer
            (integer #u64(0))
            (pointer (bytevector->pointer integer))
            ;; call the foreign function
            (code (foreign-function (cursor-handle cursor) pointer)))
       (if (eq? code 0)
           (array-ref integer 0)
           (if (eq? code WT_NOTFOUND)
               #false
               (let ((message (format #false "(cursor-search-near ~a)" cursor)))
                 (wiredtiger-string-error message code))))))))

(define-public (cursor-search-near cursor)
  ((%cursor-search-near cursor)))

(define (%cursor-insert cursor)
  (foreign
   (int (cursor-structure-insert (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(cursor-insert ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-insert cursor)
  ((%cursor-insert cursor)))

(define (%cursor-update cursor)
  (foreign
   (int (cursor-structure-update (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(cursor-update ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-update cursor)
  ((%cursor-update cursor)))

(define (%cursor-remove cursor)
  (foreign
   (int (cursor-structure-remove (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(cursor-remove ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-remove cursor)
  ((%cursor-remove cursor)))

(define (%cursor-close cursor)
  (foreign
   (int (cursor-structure-close (cursor-structure cursor)) *pointer*)
   (lambda (foreign-function)
     (let* (;; call the foreign function
            (code (foreign-function (cursor-handle cursor))))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(cursor-close ~a)" cursor)))
             (wiredtiger-string-error message code)))))))

(define-public (cursor-close cursor)
  ((%cursor-close cursor)))

;;; e.g.

;; (define connection (pk (connection-open "/tmp/wt" "create")))
;; (define session (pk (session-open connection)))

;; ;; create a table
;; (session-create session "table:nodes" "key_format=i,value_format=S")

;; ;; open a cursor over that table
;; (define cursor (pk (cursor-open session "table:nodes")))

;; ;; start a transaction and add a record
;; (session-transaction-begin session "isolation=\"snapshot\"")
;; (cursor-key-set cursor 42)
;; (cursor-value-set cursor "The one true number!")
;; (cursor-insert cursor)
;; (session-transaction-commit session)

;; (cursor-reset cursor)
;; (cursor-next cursor)
;; (pk (cursor-key-ref cursor))
;; (pk (cursor-value-ref cursor))
;; (cursor-close cursor)
;; (session-close session)
;; (connection-close connection)
