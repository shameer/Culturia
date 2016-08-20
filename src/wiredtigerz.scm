;; guile-wiredtiger - 0.4 - 2016/07/15

;; Copyright © 2014-2016 Amirouche BOUBEKKI <amirouche@hypermove.net>

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
(define-module (wiredtigerz))

(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (ice-9 optargs))

(use-modules (srfi srfi-1))  ;; append-map
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-26))  ;; cut

(use-modules (wiredtiger))


;;;
;;; plain records
;;;
;;
;; macro to quickly define records
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


;;; helpers

(define-public (scm->string scm)
  (call-with-output-string
    (lambda (port)
      (write scm port))))

(define-public (string->scm string)
  (call-with-input-string string read))

;;;
;;; wiredtigerz try to explicit main wiredtiger workflow
;;;
;;
;; This modules defines a star procedure version of wiredtiger procedure
;; to help jump into wiredtiger making the main workflow more explicit.
;;
;; The main workflow is the following:
;;
;; 1. define a some table and indices
;; 2. open session per thread
;; 3. open a single cursor per table and indices
;; 4. forward cursor navigation
;;
;; In theory you might open multiple cursors for the same table/index but this
;; leads to extra bookeeping for which I have no good reason to apply.
;;
;; The API defined as star procedure try to remains thin on top of wiredtiger
;; so that you can drop to raw wiredtiger when required, like open multiple cursors.
;;
;; This introduce a declarative API (described below) that both defines the tables
;; and the cursor in single block of code which must be used with `session-create*`
;; and `cursor-open*` which do the job described in 1. and 2.
;;
;; Also wiredtiger mainly rely on statefull API where the cursor is first configured with
;; cursor-key-set and  then an operation is executed on it like cursor-search or
;; cursor-remove. This leaves the door open for many workflows while keeping each
;; procedure signature simple.
;;
;; The purpose of the proposed (star) procedures is to simplify user code by covering
;; the main workflow (I've encountered) while leaving aside some performance concerns.
;;

;;;
;;; Declarative api
;;;
;;
;; Declare the layout of the database and its cursors using list and symbols
;; Here is two example configurations:
;;
;;   (define atoms '(atoms
;;                   ((uid . record))
;;                   ((assoc . raw))
;;                   ()))
;;
;;   (define arrows '(arrows
;;                    ((key . record))
;;                    ((start . unsigned-integer)
;;                     (end . unsigned-integer))
;;                    ;; index
;;                    '((outgoings (start) (uid end))
;;                      (incomings (end) (uid start)))))
;;
;; The format can be described as follow:
;;
;; (table-name
;;  (key assoc as (column-name . column-type))
;;  (value assoc as (column-name . column-type))
;;  (indices as (indexed name (indexed keys) (projection as column names))))
;;
;;
;; If there is no indices, the field MUST NOT be omited but replaced with an empty list
;;
;; The configuration can be used in (session-create* session . configs) to create
;; the tables and indices.
;;
;; And then in (cursor-open* session . configs) to all the table and indices cursors.
;;
;; A <context> record exists which should be associated with a thread. It encapsulates
;; a <session> and cursors.
;; A <context> can be created with (context-open connection . config).
;; Shortcuts exists to execute transaction against a context directly.
;;

;; utils for declarative configuration

(define-record-type* <config> name key value indices)
(define-record-type* <index> name keys values)

;; FIXME: some types are missing
(define (symbol->config symbol)
  (assoc-ref '((record . "r")
               (string . "S")
               (unsigned-integer . "Q")
               (positive-integer . "Q")
               (integer . "q")
               (bytes . "u"))
             symbol))


;;; define session-create*

(define-public (session-create* session . configs)
  ;; XXX: here instead of using `session-create` downstream
  ;; we wait for `session-create` arguments instead.
  ;; This makes the code easier to test...
  (define (create args)
    (apply session-create (cons session args)))
  ;; prepare arguments for every config and apply them
  (for-each create (append-map config-prepare-create configs)))

(define-public (config-prepare-create config)
  ;; a config generate one table and maybe several indices
  (cons (config-prepare-create-table config)
        (config-prepare-create-indices config)))

(define (config-prepare-create-table config)
  ;; transform declarative api into a session-create arguments
  (define (symbols->config symbols)
    (string-concatenate (map (cut symbol->config <>) symbols)))

  (define (symbols->columns symbols)
    (string-join (map (cut symbol->string <>) symbols) ","))

  (let* ((config (apply make-config config))
         (name (string-append "table:" (symbol->string (config-name config))))
         (key (symbols->config (map cdr (config-key config))))
         (value (symbols->config (map cdr (config-value config))))
         (columns (append (config-key config) (config-value config)))
         (columns (symbols->columns (map car columns)))
         (config (format #false
                         "key_format=~a,value_format=~a,columns=(~a)"
                         key value columns)))
    (list name config)))

(define (config-prepare-create-indices config)
  ;; one config may have multiple indices
  (let ((config (apply make-config config)))
    (map (config-prepare-create-index (config-name config)) (config-indices config))))

(define (config-prepare-create-index name)
  ;; convert declarative configuration to session-create arguments
  (define (symbols->columns symbols)
    (string-join (map (cut symbol->string <>) symbols) ","))

  (lambda (index)
    (let* ((index (apply make-index index))
           (name (string-append "index:" (symbol->string name) ":" (symbol->string (index-name index))))
           (columns (format #false "columns=(~a)" (symbols->columns (index-keys index)))))
      (list name columns))))

;;;
;;; define cursor-open*
;;;
;;
;; open cursor for every table and indices in an assoc where the key is
;; the table name for main cursor, '-append prefixed with the name of table
;; for the append cursor when applicable and the name index prefixed with
;; the name of the table.
;; cursor-open* will automatically create a 'append' cursor for tables
;; that have single record column.
;;

(define-public (cursor-open* session . configs)
  "Open cursors against SESSION using CONFIGS and return an assoc
with cursor symbols as key and cursors as value"
  ;; XXX: just like session-open* we expect cursor-open arguments
  ;; but this time we return an assoc made of ('cursor-name . cursor)
  (define (open name+args)
    (cons (car name+args) (apply cursor-open (cons session (cadr name+args)))))
  ;; prepare arguments for every config and apply them
  (map open (append-map config-prepare-open configs)))

(define (config-prepare-open config)
  (append (config-prepare-cursor-open config)
          (config-prepare-cursor-append-open config)
          (config-prepare-cursor-indices-open config)))

(define (config-prepare-cursor-open config)
  (let* ((config (apply make-config config))
         (name (config-name config)))
    ;; XXX: config-prepare-open expect a list of cursor-open arguments
    (list (list name (list (format #false "table:~a" name))))))

(define (config-prepare-cursor-append-open config)
  (define (key-is-record? key)
    (and (eq? (length key) 1) (eq? (cdar key) 'record)))
  (let* ((config (apply make-config config))
         (name (config-name config))
         (cursor-name (symbol-append name '-append)))
    (if (key-is-record? (config-key config))
        ;; add a append cursor over the table
        ;; XXX: config-prepare-open expect a list of cursor-open arguments
        (list (list cursor-name (list (format #false "table:~a" name) "append")))
        ;; no cursor is required
        (list))))

(define (config-prepare-cursor-indices-open config)
  (let ((config (apply make-config config)))
    (map (config-prepare-cursor-index-open (config-name config)) (config-indices config))))

(define (config-prepare-cursor-index-open name)
  (define (symbols->columns symbols)
    (string-join (map (cut symbol->string <>) symbols) ","))

  (lambda (index)
    (let* ((index (apply make-index index))
           (columns (symbols->columns (index-values index)))
           (cursor-name (symbol-append name '- (index-name index))))
      (if (equal? columns "")
          (list cursor-name
                (list (format #false "index:~a:~a" name (index-name index))))
          (list cursor-name
                (list (format #false "index:~a:~a(~a)" name (index-name index) columns)))))))

;;;
;;; <context>
;;;
;;
;; A session and cursors assoc
;;

(define-record-type* <context> session cursors)

(export context-session)
(export context-cursors)

(define-public (context-open connection . configs)
  "Open a context using CONNECTION with CONFIGS"
  (let* ((session (session-open connection))
         (cursors (apply cursor-open* (cons session configs))))
    (make-context session cursors)))

(define-public (wiredtiger-open* path . configs)
  "Open or create a database at PATH with CONFIGS and return
a two values: the connection and a context"
  (let* ((connection (connection-open path "create"))
         (session (session-open connection)))
    (apply session-create* (cons session configs))
    (values connection (make-context session (apply cursor-open* (cons session configs))))))

(define-public (context-ref context name)
  "Return the cursor associated with NAME in CONTEXT"
  (assoc-ref (context-cursors context) name))

(define-public (context-begin context)
  "Start a transaction against CONTEXT"
  (session-transaction-begin (context-session context)))

(define-public (context-commit context)
  "Commit transaction against CONTEXT"
  (session-transaction-commit (context-session context)))

(define-public (context-rollback context)
  "Rollback transaction against CONTEXT"
  (session-transaction-rollback (context-session context)))

(define-syntax-rule (with-transaction context e ...)
  (catch #true
    (lambda ()
      (context-begin context)
      (let ((out (begin e ...)))
        (context-commit context)
        out))
    (lambda (key . args)
      (context-rollback context)
      (apply throw (cons key args)))))

(export with-transaction)

;;;
;;; Cursor navigation
;;;
;;
;; Quickly operate on cursors
;;

;; helper for reseting cursors after doing some operations
;; @@@: emacs: (put 'with-cursor 'scheme-indent-function 1)
(define-syntax-rule (with-cursor cursor e ...)
  (let ((out (begin e ...)))
    (cursor-reset cursor)
    out))

(export with-cursor)

(define-syntax-rule (call-with-cursor context name proc)
  (let* ( (cursor (context-ref context name)))
    (let ((out (proc cursor)))
      (cursor-reset cursor)
        out)))


(export call-with-cursor)


(define-public (cursor-next* cursor)
  "Move the cursor to the next result and return #t.
Return #f if there is no next result (end of the table).

This procedure return a boolean instead of throwing an exception"
  (catch 'wiredtiger
    (lambda () (cursor-next cursor) #t)
    (lambda _ #f)))

(define-public (cursor-previous* cursor)
  "Move the cursor to the previous result and return #t.
Return #f if there is no previous result (end of the table).

This procedure return a boolean instead of throwing an exception"
  (catch 'wiredtiger
    (lambda () (cursor-previous cursor) #t)
    (lambda _ #f)))

(define-public (cursor-debug cursor)
  "Prints the whole table starting at cursor position"
  (pk 'cursor-debug cursor)
  (with-cursor cursor
    (let loop ((next #true))
      (when next
        (pk (cursor-key-ref cursor) (cursor-value-ref cursor))
        (loop (cursor-next* cursor))))))

(define-public (cursor-value-ref* cursor . key)
  "Search KEY and return the associated value.
Throw a 'wiredtiger error if the key is not found"
  (with-cursor cursor
    (apply cursor-search* (cons cursor key))
    (cursor-value-ref cursor)))

(define-public (cursor-insert* cursor key value)
  "Insert using CURSOR KEY and VALUE. If KEY is null,
the key will not be set and the key assigned by wiredtiger
will be returned. This is useful in the case of a table with
a single record key column."
  (unless (null? key)
    (apply cursor-key-set (cons cursor key)))
  (apply cursor-value-set (cons cursor value))
  (cursor-insert cursor)
  (when (null? key)  ;; used with single record key column
    (car (cursor-key-ref cursor))))

(define-public (cursor-update* cursor key value)
  "Update KEY with VALUE using CURSOR"
  (apply cursor-key-set (cons cursor key))
  (apply cursor-value-set (cons cursor value))
  (cursor-update cursor))

(define-public (cursor-remove* cursor . key)
  "Remove row having KEY as key using CURSOR"
  (apply cursor-key-set (cons cursor key))
  (cursor-remove cursor))

(define-public (cursor-search* cursor . key)
  "Position the  cursor at KEY. Throw a 'wiredtiger error
if KEY is not found"
  (apply cursor-key-set (cons cursor key))
  (cursor-search cursor))

(define-public (cursor-search-near* cursor . key-prefix)
  "Search near KEY-PREFIX on CURSOR"
  (apply cursor-key-set (cons cursor key-prefix))
  (cursor-search-near cursor))


;; cursor-range

(define-public (cursor-range cursor . key)
  "Return a list made of the values taken by rows having KEY as key"
  (with-cursor cursor
    (catch 'wiredtiger
      (lambda ()
        (apply cursor-search* (cons cursor key))
        (let loop ((out (list (cursor-value-ref cursor))))
          (if (cursor-next* cursor)
              (if (equal? (cursor-key-ref cursor) key)
                  (loop (cons (cursor-value-ref cursor) out))
                  out)
              out)))
      (lambda _ '()))))

;; cursor-range-prefix

(define-public (prefix? prefix other)
  "Return #true if OTHER has KEY as prefix"
  ;; filter "empty" values from the key
  (define (empty? x) (or (eq? x 0) (equal? x "") (eq? x #vu8())))
  (define (predicate? a b) (not (or (empty? a) (equal? a b))))
  (not (any predicate? prefix other)))

(define-public (cursor-range-prefix cursor . key-prefix)
  "Return CURSOR range association where keys match PREFIX"
  (define (next?)
    (catch 'wiredtiger
      (lambda ()
        (cursor-next cursor)
        (prefix? key-prefix (cursor-key-ref cursor)))
      (lambda (key . args)
        #false)))
  (with-cursor cursor
    (let ((code (catch 'wiredtiger
                  (lambda () (apply cursor-search-near* (cons cursor key-prefix)))
                  (lambda (key . args) #f))))
      (if code
          (if (or (eq? code 0) (eq? code 1) (and (eq? code -1) (cursor-next* cursor)))
              (let loop ((out (list))
                         (valid? (prefix? key-prefix (cursor-key-ref cursor))))
                (if valid?
                    (loop (acons (cursor-key-ref cursor) (cursor-value-ref cursor) out)
                          (next?))
                    out))
              '())
          '()))))

;;;
;;; generate-uid
;;;

(define (random-id size)
  "Generate and random identifier of length SIZE"
  (define CHARS "0123456789AZERTYUIOPQSDFGHJKLMWXCVBN")
  ;; append SIZE alphanumeric chars from `CHARS`
  (let loop ((count size)
             (id ""))
    (if (eq? count 0)
        id
        (loop (1- count) (format #f "~a~a" id (string-ref CHARS (random 36)))))))

(define*-public (generate-uid exists? #:optional (size 8))
  "Generate a random string made up alphanumeric ascii chars that doesn't exists
   according to `exists?`"
  (let loop ()
    ;; generate a random identifier until it find an one that doesn't already `exists?`
    (let ((id (random-id size)))
      (if (exists? id) (loop) id))))

;;;
;;; tests
;;;

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_WIREDTIGERZ"))
  (format #true "* testing wiredtigerz\n")

  ;; test declarative API

  (test-check "create table config without index"
    (config-prepare-create '(atoms
                             ((uid . record))
                             ((assoc . bytes))
                             ()))
    (list (list "table:atoms" "key_format=r,value_format=u,columns=(uid,assoc)")))

  (test-check "create table config with index and projections"
    (config-prepare-create '(arrows
                             ((key . record))
                             ((start . unsigned-integer)
                              (end . unsigned-integer))
                             ;; index
                             ((outgoings (uid,start) (uid end))
                              (incomings (end) ()))))
    (list (list "table:arrows" "key_format=r,value_format=QQ,columns=(key,start,end)")
          (list "index:arrows:outgoings" "columns=(uid,start)")
          (list "index:arrows:incomings" "columns=(end)")))

  (test-check "create cursor config without index"
    (config-prepare-open '(atoms
                           ((uid . record))
                           ((assoc . bytes))
                           ()))
    (list (list 'atoms (list "table:atoms"))
          (list 'atoms-append (list "table:atoms" "append"))))

  (test-check "create cursor config with index with and without projection"
    (config-prepare-open '(atoms
                           ((uid . record))
                           ((assoc . bytes))
                           ((reversex (assoc) (uid))
                            (reverse (assoc) ()))))
    (list (list 'atoms (list "table:atoms"))
          (list 'atoms-append (list "table:atoms" "append"))
          (list 'atoms-reversex (list "index:atoms:reversex(uid)"))
          (list 'atoms-reverse (list "index:atoms:reverse"))))

  ;; test star API

  (test-check "wiredtiger-open*"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((key . record)) ((value . integer)) ()))
      (with-cnx cnx #true))
    #true)

  (test-check "cursor-insert* and cursor-search*"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(terms ((key . record)) ((value . unsigned-integer)) ()))
      (with-cnx cnx
        (let ((append (context-ref ctx 'terms-append)))
          (cursor-insert* append #nil (list 42))
          (cursor-insert* append #nil (list 1337))
          (cursor-insert* append #nil (list 1985)))))
    3)

  (test-check "cursor-range 0"
    (receive (cnx ctx) (wiredtiger-open* "/tmp/wt" '(table ((k . record))
                                                           ((v . integer))
                                                           ((reversed (v) (k)))))
      (with-cnx cnx
        (cursor-range (context-ref ctx 'table-reversed) 42)))
    '())

  (test-check "cursor-range 1"
    (receive (cnx ctx) (wiredtiger-open* "/tmp/wt" '(table ((k . record))
                                                           ((v . integer))
                                                           ((reversed (v) (k)))))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(1))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(42))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(42))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(42))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(1))
      (with-cnx cnx
        (cursor-range (context-ref ctx 'table-reversed) 42)))
    '((4) (3) (2)))

  (test-check "cursor-range 2"
    (receive (cnx ctx) (wiredtiger-open* "/tmp/wt" '(table ((k . record))
                                                           ((v . integer))
                                                           ((reversed (v) (k)))))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(1))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(1))
      (with-cnx cnx
        (cursor-range (context-ref ctx 'table-reversed) 42)))
    '())

  (test-check "cursor-range 3"
    (receive (cnx ctx) (wiredtiger-open* "/tmp/wt" '(table ((k . record))
                                                           ((v . integer))
                                                           ((reversed (v) (k)))))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(1))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(42))
      (cursor-insert* (context-ref ctx 'table-append) #nil '(1))
      (with-cnx cnx
        (cursor-range (context-ref ctx 'table-reversed) 42)))
    '((2)))

  (test-check "cursor-range-prefix"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (cursor-insert* cursor (list 0 0) (list 0))
        (cursor-insert* cursor (list 1 1) (list 1))
        (cursor-insert* cursor (list 1 2) (list 1))
        (cursor-insert* cursor (list 2 0) (list 2))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '(((1 2) 1)
      ((1 1) 1)))

  (test-check "cursor-range-prefix 2"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (cursor-insert* cursor (list 1 1) (list 1))
        (cursor-insert* cursor (list 1 2) (list 1))
        (cursor-insert* cursor (list 2 0) (list 2))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '(((1 2) 1)
      ((1 1) 1)))

  (test-check "cursor-range-prefix 3"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (cursor-insert* cursor (list 2 0) (list 2))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '())

  (test-check "cursor-range-prefix 3"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (cursor-insert* cursor (list 0 0) (list 0))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '())

  (test-check "cursor-range-prefix 4"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (cursor-insert* cursor (list 0 0) (list 0))
        (cursor-insert* cursor (list 1 0) (list 0))
        (cursor-insert* cursor (list 1 1) (list 1))
        (cursor-insert* cursor (list 1 2) (list 1))
        (cursor-insert* cursor (list 2 0) (list 2))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '(((1 2) 1)
      ((1 1) 1)
      ((1 0) 0)))

  (test-check "cursor-range-prefix 5"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (cursor-insert* cursor (list 0 0) (list 0))
        (cursor-insert* cursor (list 1 0) (list 0))
        (cursor-insert* cursor (list 1 1) (list 1))
        (cursor-insert* cursor (list 1 2) (list 1))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '(((1 2) 1)
      ((1 1) 1)
      ((1 0) 0)))

  (test-check "cursor with empty range-prefix"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (with-cnx cnx
          (cursor-range-prefix cursor 1 0))))
    '())
  (test-check "cursor-value-ref* on empty table"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((a . integer) (b . integer)) ((c . integer)) ()))
      (let ((cursor (context-ref ctx 'table)))
        (with-cnx cnx
          (catch 'wiredtiger
            (lambda ()
              (cursor-value-ref* cursor 42))
            (lambda (key . args)
              #true)))))
    #true)
  )
