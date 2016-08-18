(define-module (hn))

(use-modules (srfi srfi-1))
(use-modules (rnrs io ports))
(use-modules (ice-9 receive))
(use-modules (ice-9 threads))
(use-modules (http))
(use-modules (json))
(use-modules (msgpack))


(define (max-id)
  (receive (response body) (http-get "https://hacker-news.firebaseio.com/v0/maxitem.json")
    (string->number body)))

(define (download uid)
  (catch #t
    (lambda ()
      (let* ((uid (1+ uid))
             (url "https://hacker-news.firebaseio.com/v0/item/~a.json")
             (url (format #f url uid)))
        (cons uid
              (call-with-output-string
               (lambda (port)
                 (write (curl url) port))))))
    (lambda _ "")))

(define (store pair)
  (if (equal? string "")
    (pk "0")
    (let ((port (open-file "hn.scm" "a")))
      (pk (car pair))
      (put-string port (cdr pair))
      (close port))))

(define (dump)
  (n-for-each-par-map 20 store download (iota 100)))

(dump)
