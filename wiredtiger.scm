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
(use-modules (rnrs arithmetic bitwise))

(use-modules (ice-9 iconv))  ;; string->bytevector
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 receive))

(use-modules (system foreign))  ;; ffi

;;;
;;; Packing
;;;
;;
;; FIXME: support all numbers
;; FIXME: maybe use compression format to have small representation for small
;;        numbers

(define-public (pack-exact-positive-integer number)
  (define (pad bytes)
    (if (eq? (length bytes) 8)
        bytes
        (pad (cons 0 bytes))))

  (if (> number (- (integer-expt 2 64) 1))
      (throw 'wiredtiger "number is bigger than 2**64-1"))

  (if (< number 0)
      (throw 'wiredtiger "number must be positive"))

  (if (not (exact-integer? number))
      (throw 'wiredtiger "number must be exact"))

  (pad (let loop ((number number)
                  (out '()))
         (if (eq? number 0)
             out
             (loop (bitwise-arithmetic-shift-right number 8)
                   (cons (bitwise-and number #xFF) out))))))

(define pack-integer pack-exact-positive-integer)

(define-public (unpack-exact-positive-integer bytes)
  (define (unpad bytes)
    (if (eq? (car bytes) 0)
        (unpad (cdr bytes))
        bytes))

  (let loop ((bytes (unpad bytes))
             (number 0))
    (if (null? bytes)
        number
        (loop (cdr bytes)
              (let ((inter (bitwise-arithmetic-shift number 8)))
                (+ inter (car bytes)))))))


(define (unpack-integer bytes)
  (values (unpack-exact-positive-integer (pk (list-head bytes 8)))
          (list-tail bytes 8)))


;;;
;;; unpack and pack
;;;

(define (pack spec . values)
  (u8-list->bytevector
   (let next ((spec (zip (string->list spec) values))
              (bytes '()))
     (if (null? spec)
         bytes
         (cond
          ;; variable length string
          ((equal? (car spec) #\S)
           (loop (cdr spec) (append out (string->u8list value) '(0))))
          ;; variable length bytevector
          ;;
          ;; XXX: because wiredtiger use U internally we have to declare it
          ;;      here because of how value are parsed in cursor procedures
          ;;      that said U should not be used in user code as it is not
          ;;      part of the public API and things can change
          ((or (equal? (car spec) #\u) (equal? (car spec) #\U))
           (loop (cdr spec) (append out
                                    (pack-integer (bytevector-length value))
                                    (bytevector->u8-list value))))

         ;; integral type
         (else (loop (cdr spec) (append out (pack-integer value)))))))))

(define (unpack-rec spec bv)
  (let loop ((spec (string->list spec))
             (bytes (bytevector->u8-list bv))
             (values '()))
    (cond ((and (null? spec) (null? bytes)) values)
          ((or (null? spec) (null? bytes))
           (throw 'wiredtiger (format #false "specification error (unpack ~a ~a)" spec bv)))
          ((equal? (car spec) #\S)
           (let* ((end (list-index bytes 0))
                  (tail (list-tail bytes (+ end 1)))
                  (head (list-head bytes end))
                  (string (bytevector->string (list->u8vector head) "utf8")))
             (loop (cdr spec) tail (cons values string))))
          ;; variable length bytevector
          ((or (equal? char #\u) (equal? char #\U))
           (throw 'wiredtiger bytes))
           ;; (receive (size bytes) (unpack-integer bytes)
           ;;   (let ((tail (list-tail bytes size))
           ;;         (head (list-head bytes size)))
           ;;     (if (equal? char #\u)
           ;;         (unpack-rec fmt tail (cons (u8-list->bytevector head) out))
           ;;         ;; for some reason U has a size prefix
           ;;         (unpack-rec fmt tail (cons (car (unpack "u" head)) out))))))
          (else ;; integral type
           (receive (value bytes) (unpack-integer bytes)
             (loop (cdr spec) (cons values value)))))))

(define-public (unpack fmt bytes)
  (if (bytevector? bytes)
      (reverse (unpack-rec fmt (bytevector->u8-list bytes) '()))
      (reverse (unpack-rec fmt bytes '()))))

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
           (let ((message (format #false "(session-close ~s)" session)))
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

(define (%session-transaction-rollback session)
  (foreign
   (int  (session-structure-transaction-rollback (session-structure session)) *pointer* *pointer*)
   (lambda (foreign-function name config)
     (let* ((%config (string->pointer config))
            ;; call the foreign function
            (code (foreign-function (session-handle session) %config)))
       (if (eq? code 0)
           #true
           (let ((message (format #false "(session-transaction-rollback ~s ~s)" session config)))
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
