;; guile-wiredtiger - 0.4 - 2016/07/07

;; Copyright © 2014-2016 Amirouche BOUBEKKI <amirouche at hypermove net>

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

;;; Comment:
;;
;; Tested with wiredtiger develop branch
;;

(define-module (wiredtiger))

(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-26)) ;; cut

(use-modules (rnrs bytevectors))

(use-modules (ice-9 iconv))  ;; string->bytevector
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))

(use-modules (system foreign))  ;; ffi

;;;
;;; plain
;;;
;;
;; macro to quickly define records
;;
;;
;; Usage:
;;
;;   (define-record-type <car> seats wheels)
;;   (define smart (make-car 2 4))
;;   (car-seats smart) ;; => 2
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

(define NULL %null-pointer)
(define POINTER '*)

(define* (dynamic-link* #:optional library-name)
  (let ((shared-object (if library-name (dynamic-link library-name) (dynamic-link))))
    (lambda (return-value function-name . arguments)
      (let ((function (dynamic-func function-name shared-object)))
        (pointer->procedure return-value function arguments)))))

(define (pointer->procedure* return-type function-pointer . args_types)
  (pointer->procedure return-type function-pointer args_types))

(define (make constructor pointer size)
  (let* ((pointer (make-pointer (array-ref pointer 0)))
         (struct (map make-pointer (u64vector->list (pointer->bytevector pointer size 0 'u64)))))
    (apply constructor (cons pointer struct))))

;;;
;;; wiredtiger bindings
;;;

(define wiredtiger (dynamic-link* "libwiredtiger.so"))

(define* wiredtiger-string-error
  (let ((function (wiredtiger POINTER "wiredtiger_strerror" int)))
    (lambda (code)
      (pointer->string (function code)))))

(define (check code)
  (unless (eq? code 0)
    (throw 'wiredtiger (wiredtiger-string-error code))))

;;;
;;; Connection
;;;

(define-record-type* <*connection>
  pointer

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

(set-record-type-printer! <*connection>
                          (lambda (record port)
                            (format port
                                    "<connection 0x~x>"
                                    (pointer-address (*connection-pointer record)))))

(define-public connection-open
  (let ((function (wiredtiger int "wiredtiger_open" POINTER POINTER POINTER POINTER)))
    (lambda (home config)
      (let* (;; init a double pointer
             (pointer (u64vector 0))
             (double-pointer (bytevector->pointer pointer))
             ;; convert arguments to c types
             (home (string->pointer home))
             (config (string->pointer config)))
      ;; FIXME: add support for error_handler
      (check (function home NULL config double-pointer))
      (make make-*connection pointer 15)))))


(define*-public (connection-close connection #:optional (config ""))
  (let ((function (pointer->procedure* int (*connection-close connection) POINTER POINTER)))
    (let* (;; init a double pointer
           (pointer (u64vector 0))
           (double-pointer (bytevector->pointer pointer))
           ;; convert arguments to c types
           (config (string->pointer config)))
           ;; FIXME: add support for error_handler
      (check (function (*connection-pointer connection) config)))))


;;;
;;; Session
;;;

(define-record-type* <*session>
  pointer

  connection
  app-private
  close
  reconfigure
  string-error
  cursor-open
  create
  compact
  drop
  join
  log-flush
  log-printf
  rebalance
  rename
  reset
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

(set-record-type-printer! <*session>
                          (lambda (record port)
                            (format port
                                    "<session 0x~x>"
                                    (pointer-address (*session-pointer record)))))

(define (session-string-error* session code)
  (let ((function (pointer->procedure* POINTER (*session-string-error session) POINTER int)))
    (pointer->string (function (*session-pointer session) code))))

(define (session-check session code)
  (unless (eq? code 0)
    (throw 'wiredtiger (session-string-error* session code))))

(define*-public (session-open connection #:optional (config ""))
  (let ((function (pointer->procedure* int (*connection-open-session connection) POINTER POINTER POINTER POINTER)))
    (let* (;; init a double pointer
           (pointer (u64vector 0))
           (double-pointer (bytevector->pointer pointer))
           ;; convert arguments to c types
           (config (string->pointer config))
           ;; call the foreign function
           ;; FIXME: add support for error_handler
           (code (function (*connection-pointer connection) NULL config double-pointer)))
      (check  code)
      (make make-*session pointer 26))))

(define-public (session-create session name config)
  (let ((function (pointer->procedure* int (*session-create session) POINTER POINTER POINTER)))
    (let* (;; convert arguments to c types
           (name (string->pointer name))
           (config (string->pointer config))
           ;; call the foreign function
           (code (function (*session-pointer session) name config)))
      (session-check session code))))

(define-public (session-close session)
  (let ((function (pointer->procedure* int (*session-close session) POINTER)))
    (session-check session (function (*session-pointer session)))))

(define*-public (session-transaction-begin session #:optional (config ""))
  (let ((function (pointer->procedure* int (*session-transaction-begin session) POINTER POINTER)))
    (session-check session (function (*session-pointer session) (string->pointer config)))))

(define*-public (session-transaction-commit session #:optional (config ""))
  (let ((function (pointer->procedure* int (*session-transaction-commit session) POINTER POINTER)))
    (session-check session (function (*session-pointer session) (string->pointer config)))))

(define*-public (session-transaction-rollback session #:optional (config ""))
  (let ((function (pointer->procedure* int (*session-transaction-rollback session) POINTER POINTER)))
    (session-check session (function (*session-pointer session) (string->pointer config)))))

;;;
;;; Cursor
;;;

(define-record-type* <*cursor>
  pointer
  session
  config

  session-pointer
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

(set-record-type-printer! <*cursor>
                          (lambda (record port)
                            (format port
                                    "<cursor 0x~x uri=~s key=~s value=~s config=~s>"
                                    (pointer-address (*cursor-pointer record))
                                    (pointer->string (*cursor-uri record))
                                    (cursor-key-format record)
                                    (cursor-value-format record)
                                    (*cursor-config record))))

(define (cursor-key-format cursor)
  ;; FIXME: cache this value
  (pointer->string (*cursor-key-format cursor)))

(define (cursor-value-format cursor)
  ;; FIXME: cache this value
  (pointer->string (*cursor-value-format cursor)))

(define*-public (cursor-open session uri #:optional (config ""))
  (let ((function (pointer->procedure* int (*session-cursor-open session) POINTER POINTER POINTER POINTER POINTER)))
    (let* (;; init a double pointer
           (pointer (u64vector 0))
           (double-pointer (bytevector->pointer pointer))
           ;; call the foreign function
           (code (function (*session-pointer session)
                           (string->pointer uri)
                           NULL
                           (string->pointer config)
                           double-pointer)))
      (session-check session code)
      ;; make cursor record
      (let* ((size 20)
             (pointer (make-pointer (array-ref pointer 0)))
             (struct (map make-pointer (u64vector->list (pointer->bytevector pointer size 0 'u64)))))
        (apply make-*cursor (cons* pointer session config struct))))))

;;; key/value set/ref

(define (item->string bv)
  (pointer->string (make-pointer (array-ref bv 0))))

(define (item->integer bv)
  (array-ref bv 0))

(define *item->value* `((#\S . ,item->string)
                        (#\Q . ,item->integer)
                        (#\q . ,item->integer)
                        (#\r . ,item->integer)))

(define (pointers->scm formats pointers)
  (let loop ((formats (string->list formats))
             (pointers pointers)
             (out '()))
    (cond
     ((and (null? formats) (null? pointers)) out)
     ((or (null? formats) (null? pointers))
      (throw 'wiredtiger "failed to ref cursor value due to format error"))
     (else (loop (cdr formats)
                 (cdr pointers)
                 (append out (list ((assoc-ref *item->value* (car formats)) (car pointers)))))))))

(define-public (cursor-key-ref cursor)
  (let* ((args (map (lambda _ (u64vector 0)) (string->list (cursor-key-format cursor))))
         (args* (cons (*cursor-pointer cursor) (map bytevector->pointer args)))
         (signature (map (lambda _ POINTER) args*))
         (function (pointer->procedure int (*cursor-key-ref cursor) signature)))
    (apply function args*)
    (pointers->scm (cursor-key-format cursor) args)))

(define-public (cursor-value-ref cursor)
  (let* ((args (map (lambda _ (u64vector 0)) (string->list (cursor-value-format cursor))))
         (args* (cons (*cursor-pointer cursor) (map bytevector->pointer args)))
         (signature (map (lambda _ POINTER) args*))
         (function (pointer->procedure int (*cursor-value-ref cursor) signature)))
    (apply function args*)
    (pointers->scm (cursor-value-format cursor) args)))

;;; set procedures

(define make-string-pointer
  (compose bytevector->pointer
           (cut string->bytevector <> "utf-8")
           (cut string-append <> "\0")))


(define *format->pointer* `((#\S . ,make-string-pointer)
                            (#\Q . ,make-pointer)
                            (#\q . ,make-pointer)
                            (#\r . ,make-pointer)))

(define (formats->items formats values)
  (let loop ((formats (string->list formats))
             (values values)
             (out '()))
    (cond
     ((and (null? formats) (null? values)) out)
     ((or (null? formats) (null? values))
      (throw 'wiredtiger "failed to set cursor due to format error"))
     (else (loop (cdr formats)
                 (cdr values)
                 (append out (list ((assoc-ref *format->pointer* (car formats)) (car values)))))))))

(define-public (cursor-key-set cursor . key)
  (let* ((args (cons (*cursor-pointer cursor) (formats->items (cursor-key-format cursor) key)))
         (signature (map (lambda ignore POINTER) args))
         (function (pointer->procedure int (*cursor-key-set cursor) signature)))
    (apply function args)))

(define-public (cursor-value-set cursor . value)
  (let* ((args (cons (*cursor-pointer cursor) (formats->items (cursor-value-format cursor) value)))
         (signature (map (lambda ignore POINTER) args))
         (function (pointer->procedure int (*cursor-value-set cursor) signature)))
    (apply function args)))

(define-public (cursor-reset cursor)
  (let ((function (pointer->procedure* int (*cursor-reset cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-next cursor)
  (let ((function (pointer->procedure* int (*cursor-next cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-previous cursor)
  (let ((function (pointer->procedure* int (*cursor-previous cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-search cursor)
  (let ((function (pointer->procedure* int (*cursor-search cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-search-near cursor)
  (let ((function (pointer->procedure* int (*cursor-search-near cursor) POINTER POINTER)))
    (let* ((integer (s32vector 0))
           (pointer (bytevector->pointer integer)))
      (session-check (*cursor-session cursor) (function (*cursor-pointer cursor) pointer))
      (s32vector-ref integer 0))))

(define-public (cursor-insert cursor)
  (let ((function (pointer->procedure* int (*cursor-insert cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-update cursor)
  (let ((function (pointer->procedure* int (*cursor-update cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-remove cursor)
  (let ((function (pointer->procedure* int (*cursor-remove cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-close cursor)
  (let ((function (pointer->procedure* int (*cursor-close cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

;;;
;;; tests
;;;

(define (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be  prepend"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))


(define (path-dfs-walk dirpath proc)
  (define dir (opendir dirpath))
  (let loop ()
    (let ((entry (readdir dir)))
      (cond
       ((eof-object? entry))
       ((or (equal? entry ".") (equal? entry "..")) (loop))
       (else (let ((path (path-join dirpath entry)))
               (if (equal? (stat:type (stat path)) 'directory)
                   (begin (path-dfs-walk path proc)
                          (proc path))
                   (begin (proc path) (loop))))))))
  (closedir dir)
  (proc (path-join dirpath)))


(define (rmtree path)
  (path-dfs-walk path (lambda (path)
                        (if (equal? (stat:type (stat path)) 'directory)
                            (rmdir path)
                            (delete-file path)))))


(define-syntax-rule (with-directory path e ...)
  (begin
    (when (access? path F_OK)
      (rmtree path))
    (mkdir path)
    e ...
    (rmtree path)))


(define-syntax-rule (with-cnx cnx e ...)
  (let ((out (begin e ...)))
    (connection-close cnx)
    out))

;;; test-check

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (with-directory "/tmp/wt"
                     (format #t "** Checking ~a\n" title)
                     (let* ((expected expected-result)
                            (produced tested-expression))
                       (if (not (equal? expected produced))
                           (begin (format #t "*** Expected: ~s\n" expected)
                                  (format #t "*** Computed: ~s\n" produced))))))))


(when (or (getenv "CHECK") (getenv "CHECK_WIREDTIGER"))
  (format #true "* testing wiredtiger\n")

  (test-check "create and close database"
              (with-cnx (connection-open "/tmp/wt" "create") #true)
              #true)

  (test-check "table with index, insert and index value"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                ;; create a table
                (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
                (session-create session "index:nodes:index" "columns=(b,c)")
                ;; open a cursor over that table
                (let ((cursor (cursor-open session "table:nodes")))
                  (session-transaction-begin session "isolation=\"snapshot\"")
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "a" "b")
                  (cursor-insert cursor)
                  (session-transaction-commit session)
                  (let ((index (cursor-open session "index:nodes:index(a)")))
                    (cursor-next index)
                    (with-cnx cnx
                      (list (cursor-key-ref index) (cursor-value-ref index))))))
              (list (list "a" "b") (list 42)))

  (test-check "cursor search on empty table"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                ;; create a table
                (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
                ;; open a cursor over that table
                (let ((cursor (cursor-open session "table:nodes")))
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                    (catch 'wiredtiger
                      (lambda () (cursor-search cursor) #false)
                      (lambda (key value) #true)))))
              #true)

  (test-check "cursor search"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                ;; create a table
                (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
                ;; open a cursor over that table
                (let ((cursor (cursor-open session "table:nodes")))
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "b" "c")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                    (catch #true
                      (lambda () (cursor-search cursor) #true)
                      (lambda _ #false)))))
              #true)

  (test-check "cursor search near on empty table"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                            (catch #true
                              (lambda () (cursor-search-near cursor) #false)
                              (lambda _ #true)))))
              #true)

  (test-check "cursor search near below"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  ;; prepare
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "magic number")
                  (cursor-insert cursor)
                  ;; test
                  (cursor-key-set cursor 43)
                  (with-cnx cnx
                    (cursor-search-near cursor))))
              -1)

  (test-check "cursor search near above"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  ;; prepare
                  (cursor-key-set cursor 41)
                  (cursor-value-set cursor "another number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "magic number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 45)
                  (cursor-value-set cursor "random number")
                  (cursor-insert cursor)
                  ;; test
                  (cursor-key-set cursor 43)
                  (with-cnx cnx
                    (< 0 (cursor-search-near cursor)))))
              #true)

  (test-check "cursor search near exact match"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  ;; prepare
                  (cursor-key-set cursor 41)
                  (cursor-value-set cursor "another number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "magic number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 45)
                  (cursor-value-set cursor "random number")
                  (cursor-insert cursor)
                  ;; test
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                    (cursor-search-near cursor))))
              0)

  (test-check "record table, insert and retrieve key"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:terms" "key_format=r,value_format=S")
                (let ((cursor (cursor-open session "table:terms" "append")))
                  (cursor-value-set cursor "timesink")
                  (cursor-insert cursor)
                  (with-cnx cnx (car (cursor-key-ref cursor)))))
              1)
  )