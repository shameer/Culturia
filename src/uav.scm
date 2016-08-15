#! /bin/sh
# -*- scheme -*-
exec guile -L $(dirname $(dirname $0)) -e '(uav)' -s $0 "$@"
!#
(define-module (uav))

(use-modules (ice-9 match))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(use-modules (minikanren))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))



(set! *random-state* (random-state-from-platform))

;;;
;;; UAV tuples spec
;;;

(define-public *tuples* '(tuples
                          ((uid . string) (attribute . string))
                          ((value . string))
                          ((index (attribute value) (uid)))))

;;; open, context and fluid shortcuts

(define-public *context* (make-unbound-fluid))

(define-public (uav-open path)
  (define connection (connection-open path "create"))
  (define session (session-open connection))
  (session-create* session *tuples*)
  (session-close session)
  connection)

(define-public (uav-context-open! connection)
  (let ((context (context-open connection *tuples*)))
    (fluid-set! *context* context)
    context))

(define-public (uav-open* path)
  (let ((connection (uav-open path)))
    (uav-context-open! connection)
    connection))

(define-syntax-rule (with-transaction* e ...)
  (with-transaction (fluid-ref *context*)
                    e ...))

(export with-transaction*)

;;; database procedures

(define-public (uav-debug)
  (let* ((context (fluid-ref *context*))
         (cursor (context-ref context 'tuples)))
    (cursor-key-set cursor "" "")
    (let loop ((next (cursor-search-near cursor)))
      (when next
        (let ((key (cursor-key-ref cursor))
              (value (car (cursor-value-ref cursor))))
          (pk (car key) (string->scm (cadr key)) (string->scm value)))
        (loop (cursor-next cursor))))))

(define-public (uav-ref uid attribute)
  (let* ((context (fluid-ref *context*))
         (cursor (context-ref context 'tuples))
         (value (cursor-value-ref* cursor uid (scm->string attribute))))
    (if (null? value)
        #nil
        (string->scm (car value)))))

(define (assocify range)
  (map (match-lambda (((uid attribute) . (value))
                      (cons (string->scm attribute) (string->scm value))))
       range))

(define-public (uav-ref* uid)
  (let* ((context (fluid-ref *context*))
         (cursor (context-ref context 'tuples))
         (assoc (assocify (cursor-range-prefix cursor uid ""))))
    assoc))

(define (make-uid)
  (generate-uid (lambda (id) (not (null? (uav-ref* id))))))

(define-public (uav-add! assoc)
  (let* ((context (fluid-ref *context*))
         (uid (make-uid))
         (cursor (context-ref context 'tuples)))

    (for-each (lambda (pair)
                (cursor-insert* cursor
                                (list uid (scm->string (car pair)))
                                (list (scm->string (cdr pair)))))
              assoc)
    uid))

(define-public (uav-del! uid)
  (let* ((context (fluid-ref *context*))
         (cursor (context-ref context 'tuples))
         (assoc (alist-delete 'uid (uav-ref* uid))))
    (for-each (match-lambda
                ((attribute . value)
                 (cursor-remove* cursor uid (scm->string attribute))))
              assoc)))

(define-public (uav-update! uid assoc)
  (let* ((context (fluid-ref *context*)))
    (uav-del! uid)
    (for-each (lambda (pair)
                (cursor-insert* (context-ref context 'tuples)
                                (list uid (scm->string (car pair)))
                                (list (scm->string (cdr pair)))))
              assoc)))


(define-public (uav-index-ref attribute value)
  (let* ((context (fluid-ref *context*))
         (cursor (context-ref context 'tuples-index)))
    (map car (cursor-range cursor (scm->string attribute) (scm->string value)))))

;;; minikaren extension for querying the uav database

(define (disj* . gs)
  "non-macro disj* macro"
  (if (null? gs) (lambda (s/c) '())
      (disj (car gs) (apply disj* (cdr gs)))))

(define-public uav-refo
  (lambda (uid key value^)
    (lambda (s/c)
      ((== value^ (uav-ref (walk uid (car s/c)) key)) s/c))))

(define-public uav-index-refo
  (lambda (uid^ key value)
    (let ((uids (uav-index-ref key value)))
      (apply disj* (map (lambda (uid) (== uid uid^)) uids)))))

(define-public (query context)
  (lambda (u a v)
    (lambda (s/c)
      (let ((u (walk u (car s/c)))
            (v (walk v (car s/c))))
        (cond
         ((var? u) ((uav-index-refo u a v) s/c))
         ((var? v) ((uav-refo u a v) s/c))
         ((throw 'uav "unsupported query type")))))))


;;;
;;; uav/query
;;;
;;
;; tuple syntax to query database
;;

(define-syntax uav/query
  (lambda (x)
    (define (is-not-free-var? sym)
      (let ((sym (symbol->string sym)))
        (eq? (string-rindex sym #\?) (- (string-length sym) 1))))
    (define (make-vars ctx names tuples)
      (let* ((names (map syntax->datum names))
             (vals (map syntax->datum tuples))
             (syms (filter symbol? (concatenate vals)))
             (syms (filter is-not-free-var? syms))
             (syms (delete-duplicates syms))
             (vars (fold (lambda (x out)
                           (remove (cut equal? x <>) out))
                         syms
                         names)))
        (datum->syntax ctx vars)))

    (syntax-case x (:where)
      ((uav/query names ... :where ((u a v) ...))
       (with-syntax ((vars (make-vars x #'(names ...) #'((u a v) ...))))
         #'(reify-all (lambda (names ...)
                        (fresh vars
                           ((query *context*) u a v) ...))
                      'names ...))))))

(export uav/query)

;;;
;;; client/server
;;;
;;
;; Spawn a UAV database server with `(uav-server path port)` and query
;; from the same machine using `((uav-call port) proc . args)`.
;;
;; `uav-call` takes as `PROC` any *quoted lambda* to be evaled by the
;; server in the module environnement of the server ie. this
;; module. You can use any procedure from `wiredtiger`, `wiredtigerz`
;; and `uav` modules. You can also use any scheme construct!
;;
;; Don't expose the server to the outside world or you are doomed!
;;

;; client

(define make-socket socket)

(define (make-client-socket port)
  (let ((socket (make-socket PF_INET SOCK_STREAM 0)))
    (connect socket AF_INET INADDR_LOOPBACK port)
    socket))

(define-public (uav-call port)
  (lambda (proc . args)
    (let ((socket (make-client-socket port)))
      (write (list proc args) socket)
      (let ((out (read socket)))
        (close socket)
        out))))


;; server

(define (make-server-socket port)
  (let ((socket (make-socket PF_INET SOCK_STREAM 0)))
    (setsockopt socket SOL_SOCKET SO_REUSEADDR 1)
    (bind socket (make-socket-address AF_INET (inet-pton AF_INET "127.0.0.1") port))
    (listen socket 128)
    socket))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

(define (handler client)
  (let* ((input (read client))
         (proc (list-ref input 0))
         (args (list-ref input 1)))
    (let ((out (apply (eval proc (resolve-module '(uav))) args)))
      (write out client))))

(define-public (uav-server path port)
  (let* ((uav (uav-open* path))
         (socket (make-server-socket port)))
    (format #t "* Accepting connexion on port ~s\n" port)
    (call-with-sigint
     (lambda ()
       (while #true
         (let ((client (car (accept socket))))
           (handler client)
           (close client))))
     (lambda ()
       (display "* Exiting...\n")
       (close socket)
       (connection-close uav)))))

(define-public (main args)
  (let ((port (string->number (list-ref args 1))))
    (uav-server "db" port)))


;;; tests

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_UAV"))
  (format #t "* testing uav\n"))
  
