(use-modules (ice-9 threads))
(use-modules (http))
(use-modules (ice-9 rdelim))


;;;
;;; Input must be one url per line.  For each url, output a pair made
;;; of the url and `curl -is ,url' output
;;;


(define (maybe-curl url)
  (catch #t
    (lambda () (cons url (curl url)))
    (lambda _ '())))


(define urls (let loop ((line (read-line))
                        (out '()))
               (if (eof-object? line)
                   out
                   (loop (read-line) (cons line out)))))

(n-for-each-par-map 16 write maybe-curl urls)
