(define-module (wiredtigerz))

(use-modules (ice-9 match))
(use-modules (ice-9 receive))

(use-modules (srfi srfi-1))  ;; append-map
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-26))  ;; cut

(use-modules (srfi-99))

(use-modules (wiredtiger))

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
;; (table name
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


;; utils for declarative configuration

(define-record-type* <config> name key value indices)
(define-record-type* <index> name keys values)

;; FIXME: some types are missing
(define (symbol->config symbol)
  (assoc-ref '((record . "r")
               (string . "S")
               (unsigned-integer . "Q")
               (integer . "q")
               (raw . "u"))
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
;;; database-create
;;;
;;
;; Create database and return a connection with its cursors
;;

(define (wiredtiger-open path config)
  (let* ((connection (connection-open path "create"))
         (session (session-open connection)))
    (session-create* session config)
    (values (cons connection session) (cursor-open* session config))))


(define (wiredtiger-close database)
  (connection-close (car database)))

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


(define-public (cursor-value-ref* cursor . key)
  (with-cursor cursor
    (apply cursor-key-set (cons cursor key))
    (if (cursor-search cursor)
        (cursor-value-ref cursor)
        (list))))


(define-public (cursor-insert* cursor key value)
  (when (not (null? key))
    (apply cursor-key-set (cons cursor key)))
  (apply cursor-value-set (cons cursor value))
  (cursor-insert cursor))


(define-public (cursor-update* cursor key value)
  (when (not (null? key))
    (apply cursor-key-set (cons cursor key)))
  (apply cursor-value-set (cons cursor value))
  (cursor-update cursor))


(define-public (cursor-remove* cursor . key)
  (apply cursor-key-set (cons cursor key))
  (cursor-remove cursor))


(define-public (cursor-search* cursor . key)
  (apply cursor-key-set (cons cursor key))
  (cursor-search cursor))


(define-public (cursor-search-near* cursor . key-prefix)
  "Search near KEY on CURSOR and prepare a forward range"
  (apply cursor-key-set (cons cursor key-prefix))
  (let ((code (cursor-search-near cursor)))
    (cond
     ;; position the cursor at the first key
     ((eq? code -1) (if (cursor-next cursor) #true #false))
     ;; not found
     ((eq? code #false) #false)
     ;; 0 the correct position or 1 which means above and should be filtered
     ;; if above the key-prefix range
     (else #true))))


(define-public (prefix? prefix other)
  "Return #true if OTHER has KEY as prefix"
  ;; filter "empty" values from the key
  (define (empty? x) (or (eq? x 0) (equal? x "") (eq? x #vu8())))
  (define (predicate? a b) (not (or (empty? a) (equal? a b))))
  (not (any predicate? prefix other)))


(define-public (cursor-range cursor . key-prefix)
  "Return CURSOR range association where keys match PREFIX"
  (define (next?)
    (if (cursor-next cursor)
        (prefix? key-prefix (cursor-key-ref cursor))
        #false))

  (with-cursor cursor
    (if (apply cursor-search-near* (cons cursor key-prefix))
        (let loop ((out (list))
                   (next #true))
          (if next
              (loop (acons (cursor-key-ref cursor) (cursor-value-ref cursor) out)
                    (next?))
              out))
        (list))))


;;;
;;; tests
;;;

(use-modules (tools))  ;; test-check
(use-modules (path))  ;; with-directory
(use-modules (ice-9 receive))


(when (getenv "CHECK")

  ;;; test declarative API

  (test-check "create table config without index"
              (config-prepare-create '(atoms
                                       ((uid . record))
                                       ((assoc . raw))
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
                                     ((assoc . raw))
                                     ()))
              (list (list 'atoms (list "table:atoms"))
                    (list 'atoms-append (list "table:atoms" "append"))))

  (test-check "create cursor config with index with and without projection"
              (config-prepare-open '(atoms
                                     ((uid . record))
                                     ((assoc . raw))
                                     ((reversex (assoc) (uid))
                                      (reverse (assoc) ()))))
              (list (list 'atoms (list "table:atoms"))
                    (list 'atoms-append (list "table:atoms" "append"))
                    (list 'atoms-reversex (list "index:atoms:reversex(uid)"))
                    (list 'atoms-reverse (list "index:atoms:reverse"))))

  (with-directory
   "/tmp/culturia" (receive (db cursors)
                       (wiredtiger-open "/tmp/culturia"
                                        '(table ((key . record)) ((value . integer)) ()))
                     (test-check "wiredtiger-open"
                                 db
                                 db)
                    (wiredtiger-close db)))

  (with-directory
   "/tmp/culturia" (receive (db cursors)
                       (wiredtiger-open "/tmp/culturia"
                                        '(table ((key . record)) ((value . integer)) ()))
                     (test-check "cursor-insert* and cursor-search*"
                                 (let ((cursor (assoc-ref cursors 'table))
                                       (append (assoc-ref cursors 'table-append)))
                                   (cursor-insert* append #nil (list 42))
                                   (cursor-insert* append #nil (list 1337))
                                   (cursor-insert* append #nil (list 1985))
                                   (cursor-search* cursor 1)
                                   (cursor-value-ref cursor))
                                 (list 42))
                    (wiredtiger-close db)))


  (with-directory
   "/tmp/culturia" (receive (db cursors)
                       (wiredtiger-open "/tmp/culturia"
                                        '(table ((a . integer) (b . integer)) ((c . integer)) ()))
                     (test-check "cursor-range"
                                 (let ((cursor (assoc-ref cursors 'table)))
                                   (cursor-insert* cursor (list 0 0) (list 0))
                                   (cursor-insert* cursor (list 1 1) (list 1))
                                   (cursor-insert* cursor (list 1 2) (list 1))
                                   (cursor-insert* cursor (list 2 0) (list 2))
                                   (cursor-range cursor 1 0))
                                 '(((1 2) 1)
                                   ((1 1) 1)))
                    (wiredtiger-close db)))

  (with-directory
   "/tmp/culturia" (receive (db cursors)
                       (wiredtiger-open "/tmp/culturia"
                                        '(table ((a . integer) (b . integer)) ((c . integer)) ()))
                     (test-check "cursor-range 2"
                                 (let ((cursor (assoc-ref cursors 'table)))
                                   (cursor-insert* cursor (list 1 1) (list 1))
                                   (cursor-insert* cursor (list 1 2) (list 1))
                                   (cursor-insert* cursor (list 2 0) (list 2))
                                   (cursor-range cursor 1 0))
                                 '(((1 2) 1)
                                   ((1 1) 1)))
                    (wiredtiger-close db)))
  (with-directory
   "/tmp/culturia" (receive (db cursors)
                       (wiredtiger-open "/tmp/culturia"
                                        '(table ((a . integer) (b . integer)) ((c . integer)) ()))
                     (test-check "cursor-range 3"
                                 (let ((cursor (assoc-ref cursors 'table)))
                                   (cursor-insert* cursor (list 2 0) (list 2))
                                   (cursor-range cursor 1 0))
                                 '())
                     (wiredtiger-close db)))
  
  (with-directory
   "/tmp/culturia" (receive (db cursors)
                       (wiredtiger-open "/tmp/culturia"
                                        '(table ((a . integer) (b . integer)) ((c . integer)) ()))
                     (test-check "cursor-range 3"
                                 (let ((cursor (assoc-ref cursors 'table)))
                                   (cursor-insert* cursor (list 0 0) (list 0))
                                   (cursor-range cursor 1 0))
                                 '())
                    (wiredtiger-close db)))
  )
