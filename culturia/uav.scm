(define-module (uav))

(use-modules (ice-9 match))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (minikanren))


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
  (define connection (connection-open path "create,log={enabled=true,recover=on}"))
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
        (pk 'key (cursor-key-ref cursor))
        (pk 'value (cursor-value-ref cursor))
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
         (assoc (assocify (cursor-range cursor uid ""))))
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
    (map (match-lambda ((head uid) uid))
         (cursor-range cursor (scm->string attribute) (scm->string value)))))


;;; minikaren extensions for querying the uav database

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
;;; query*
;;;
;;
;; tuple syntax to query database
;;

(define-syntax query*
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
      ((query* names ... :where ((u a v) ...))
       (with-syntax ((vars (make-vars x #'(names ...) #'((u a v) ...))))
         #'(reify-all (lambda (names ...)
                        (fresh vars
                           ((query *context*) u a v) ...))
                      'names ...))))))

(export query*)
