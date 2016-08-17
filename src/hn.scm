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


(define (assocify ht)
  (hash-map->list cons ht))

(define (download uid)
  (catch #t
    (lambda ()
      (let* ((url "https://hacker-news.firebaseio.com/v0/item/~a.json")
             (url (format #f url (1+ uid))))
        (pack (curl url))))
    (lambda _ '())))


(define (store bv)
  (if (null? bv)
    (display "0")
    (let ((port (open-file "hn.msgpack" "ab")))
      (display "1")
      (put-bytevector port bv)
      (close port))))

(define (dump)
  (n-for-each-par-map 8 store download (iota (max-id))))

;; (define (dump)
;;   (store (download 1)))

;; (define (load)
;;   (let* ((filename "hn.msgpack")
;;          (file (open filename  O_RDONLY)))
;;     (let next-entry ((entry (get-unpack file)))
;;       (unless (eof-object? entry)
;;         (pk entry)
;;         (next-entry (get-unpack file))))))

(dump)
;; (load)
        
