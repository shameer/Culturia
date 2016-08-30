(define-module (ukv))

(use-modules (ice-9 match))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))


(set! *random-state* (random-state-from-platform))

(setlocale LC_ALL "")

;;;
;;; UKV ukv spec
;;;

(define *ukv* '(ukv
                ((uid . string) (key . string))
                ((value . string))
                ((index (key value) (uid)))))


(define-public (ukv-env-open path)
  "open and init an environment for ukv"
  (let ((env (env-open path)))
    (env-config-add env *ukv*)
    (env-create env)
    env))

(define-public (ukv-ref uid key)
  (call-with-cursor 'ukv
    (lambda (cursor)
      (car (cursor-value-ref* cursor key)))))

(define assocify
  (match-lambda (((uid key) . (value))
                 (cons (string->symbol key) (string->scm value)))))

(define (ukv-ref* uid)
  (call-with-cursor 'ukv
    (lambda (cursor)
      (map assocify (cursor-range-prefix cursor uid "")))))

(define (make-uid)
  (generate-uid (lambda (id) (not (null? (ukv-ref* id))))))

(define (ukv-add! assoc)
  (let ((uid (make-uid)))
    (call-with-cursor 'ukv
      (lambda (cursor)
        (for-each (match-lambda ((key . value)
                                 (cursor-insert* cursor
                                                 (list uid (symbol->string key))
                                                 (list (scm->string value)))))
                  assoc)))
    uid))

;;;
;;; tests
;;;

(use-modules (test-check))

(define-syntax-rule (timeit body ...)
  (let ((start (current-time)))
    (begin body ...)
    (- (current-time) start)))


(define (generate-random-assoc)
  (map (lambda (_) (cons (string->symbol (generate-uid (lambda (_) #f))) (generate-uid (lambda (_) #f)))) (iota 10)))

(when (or (getenv "CHECK") (getenv "CHECK_UKV"))
  (format #true "* testing ukv\n")

  (test-check "open ukv database"
    (with-env (ukv-env-open "/tmp/wt")
      42)
    42)

  (test-check "ukv insert 10000"
    (with-env (ukv-env-open "/tmp/wt")
      (timeit 
       (let ((uids '()))
         (let loop ((counter 10000))
           (unless (zero? counter)
             (let ((uid (ukv-add! (generate-random-assoc))))
               (set! uids (cons uid uids))
               (loop (1- counter)))))
         (for-each ukv-ref* uids))))
    '())

  )

  

