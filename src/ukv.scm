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

(define-public *ukv* '(ukv
                       ((uid . string) (key . string))
                       ((value . string))
                       ((index (key value) (uid)))))

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

(define-public (ukv-del! uid)
  (call-with-cursor 'ukv
    (lambda (cursor)
      (for-each (match-lambda
                  ((key . value)
                   (cursor-remove* cursor uid (symbol->string key))))
                (ukv-ref* uid)))))

(define-public (ukv-update! uid assoc)
  (ukv-del! uid)
  (call-with-cursor 'ukv
    (lambda (cursor)
      (for-each (match-lambda
                 ((key . value)
                  (cursor-insert* cursor
                                  (list uid (symbol->string key))
                                  (list (scm->string value)))))
              assoc))))


(define-public (ukv-index-ref key value)
  (call-with-cursor 'ukv-index
    (lambda (cursor)
      (map car (cursor-range cursor (symbol->string key) (scm->string value))))))

;;;
;;; tests
;;;

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_UKV"))
  (format #true "* testing ukv\n")

  (test-check "open ukv database"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      42)
    42)

  (test-check "ukv-index-ref"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (ukv-add! '((a . 42) (b . 1337)))
      (ukv-add! '((a . 0) (c . 1337)))
      (length (ukv-index-ref 'b 1337)))
    1)

  (test-check "ukv-del!"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (ukv-del! (ukv-add! '((a . 42) (b . 1337))))
      (length (ukv-index-ref 'b 1337)))
    0)

  (test-check "ukv-update!"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (ukv-update! (ukv-add! '((a . 42) (b . 1337))) '((a . 42)))
      (length (ukv-index-ref 'b 1337)))
    0)
  )
