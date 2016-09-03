(use-modules (ukv))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))

(setlocale LC_ALL "")


(define-syntax-rule (timeit body ...)
  (let ((start (current-time)))
    (begin body ...)
    (- (current-time) start)))


(define (generate-random-assoc)
  (map (lambda (_) (cons (string->symbol (random-string 30))
                         (random-string 128)))
       (iota 10)))

(define alpha 10000)
(define beta 10)

(define (choices lst)
  (let ((size (length lst)))
    (let loop ((counter alpha)
               (out '()))
      (if (zero? counter)
          out
          (loop (1- counter) (cons (list-ref lst (random size)) out))))))

(with-env (env-open* "/tmp/wt" (list *ukv*))
  (let next ((step 1))
    (unless (eq? step beta)
      (let ((uids '()))
        (let ((dw (timeit
                   (let loop ((counter alpha))
                     (unless (zero? counter)
                       (let ((uid (ukv-add! (generate-random-assoc))))
                         (set! uids (cons uid uids))
                         (loop (1- counter))))))))
          (let ((uids (choices uids)))
            (let ((dr (timeit (for-each ukv-ref* uids))))
              (format #t "~a ~f ~f\n"
                      (* step alpha)
                      (exact->inexact (/ alpha dw))
                      (exact->inexact (/ alpha dr)))))))
      (next (1+ step)))))
    
