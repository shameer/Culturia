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
        (receive (response body) (http-get url)
          (pack (alist-delete "kids" (assocify (json-string->scm body)) equal?)))))
    (lambda _ (pk _) '())))


(define (store bv)
  (unless (null? bv)
    (display ".")
    (let ((port (open-file "hn.msgpack" "ab")))
      (put-bytevector port bv)
      (close port))))

(define (dump)
  (n-for-each-par-map 8 store download (max-id)))

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
        
