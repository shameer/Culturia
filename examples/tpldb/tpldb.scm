(define-module (tpldb))

(use-modules (srfi srfi-1))  ;; lists
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-field

(use-modules (rnrs bytevectors))

(use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 receive))  ;; receive
(use-modules (ice-9 match))  ;; match

(use-modules (wiredtiger))

;;;
;;; Guile helpers
;;;

;; match helper
(define-syntax-rule (match-let (value query) e ...)
  (match query (value e ...)))

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
    (define (%id-name name) (string->symbol (string-drop
                                             (string-drop-right
                                              (symbol->string name) 1) 1)))
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

;;; helper for reseting cursors should be in wiredtiger
;; @@@: emacs: (put 'with-cursor 'scheme-indent-function 1)
(define-syntax-rule (with-cursor cursor e ...)
  (let ((out e ...))
    (cursor-reset cursor)
    out))

;;;
;;; generate-uid
;;;

;; init random with a random state

(set! *random-state* (random-state-from-platform))

(define-public (generate-uid exists?)
  "Generate a random string made up alphanumeric ascii chars that doesn't exists
   according to `exists?`"
  (define (random-id)
    (define CHARS "0123456789AZERTYUIOPQSDFGHJKLMWXCVBN")
    ;; append 8 alphanumeric chars from `CHARS`
    (let loop ((count 8)
               (id ""))
      (if (eq? count 0)
          id
          (loop (1- count) (format #f "~a~a" id (string-ref CHARS (random 36)))))))

  (let loop ()
    ;; generate a random uid until it find an id that doesn't already exists?
    (let ((id (random-id)))
      (if (exists? id) (loop) id))))

;;;
;;; tpldb
;;;

(define-record-type* <tpldb> connection session iav avi)

(export tpldb-iav tpldb-avi)

(define-public (create-tpldb path)
  (define connection (connection-open path "create"))
  (define session (session-open connection))

  (session-create session
                  "table:iav"
                  "key_format=SS,value_format=u,columns=(i,a,v)")
  (session-create session "index:iav:avi" "columns=(a,v,i)")
  (make-tpldb connection
                session
                (cursor-open session "table:iav")
                (cursor-open session "index:iav:avi")))

(define-public (tpldb-begin tpldb)
  (session-transaction-begin (tpldb-session tpldb)))

(define-public (tpldb-commit tpldb)
  (session-transaction-commit (tpldb-session tpldb)))

(define-public (tpldb-rollback tpldb)
  (session-transaction-rollback (tpldb-session tpldb)))

(define-public (tpldb-close tpldb)
  (connection-close (tpldb-connection tpldb)))

;;

;; prefix? helper
(define (not-empty? x) (not (or (null? x) (equal? x "") (equal? x #vu8()))))

(define (prefix? key other)
  (define prefix (filter not-empty? key))
  ;; Check that OTHER has KEY as prefix using `equal?`
  (let loop ((query (zip prefix other)))
    (if (null? query)
        #true
        (match-let [(a b) (car query)]
          (if (equal? a b)
              (loop (cdr query))
              #false)))))

(define-public (tpldb-cursor-map cursor proc . key)
  "map a CURSOR filtered using KEY values"
  ;; set cursor for search
  (apply cursor-key-set (append (list cursor) key))
  (with-cursor cursor
    (let ((code (cursor-search-near cursor)))
      (if code
          (if (cursor-next cursor)
              (let loop ((out '()))  ;; retrieve all results
                (let ((record-key (cursor-key-ref cursor)))
                  (if (prefix? key record-key)
                      (let ((out (cons (proc record-key) out)))
                        (if (cursor-next cursor)
                            (loop out)
                            out))
                       out)))
              '())
          '()))))

(define*-public (tpldb-iav-map tpldb proc uid #:optional (attribute ""))
  (tpldb-cursor-map (tpldb-iav tpldb)
                      (lambda (key)
                        (let* ((value (cursor-value-ref (tpldb-iav tpldb)))
                               (value (bytevector->scm (car value))))
                          (apply proc (append key (list value)))))
                      uid
                      attribute))

(define*-public (tpldb-avi-map tpldb proc attribute #:optional (value #vu8()))
  (tpldb-cursor-map (tpldb-avi tpldb)
                    (lambda (key)
                      (match-let ((a v i) key)
                        (proc (list a (bytevector->scm v)))))
                    attribute
                    value
                    ""))

;;

(define-public (tpldb-ref tpldb uid)
  (tpldb-cursor-map (tpldb-iav tpldb)
                    (lambda (key)
                      (cons (cadr key)  ;; attribute
                            (car (cursor-value-ref (tpldb-iav tpldb)))))
                    uid
                    ""))

;;

(define-public (tpldb-make-uid tpldb)
  (generate-uid (lambda (identifier) (not (null? (tpldb-ref tpldb identifier))))))

;; add

(define-public (tpldb-add tpldb uid attribute value)
  (cursor-key-set (tpldb-iav tpldb) uid attribute)
  (cursor-value-set (tpldb-iav tpldb) (scm->bytevector value))
  (cursor-insert (tpldb-iav tpldb))
  ;; XXX: is cursor-reset required here?
  (cursor-reset (tpldb-iav tpldb)))

(define-public (tpldb-del tpldb uid)
  (cursor-map (tpldb-iav tpldb) (lambda (key) (cursor-remove (tpldb-iav tpldb))) uid ""))

