(define-module (hn))

(use-modules (rnrs io ports))
(use-modules (ice-9 receive))
(use-modules (http))
(use-modules (json))
(use-modules (msgpack))


(define (max-id)
  (receive (response body) (http-get "https://hacker-news.firebaseio.com/v0/maxitem.json")
    (string->number body)))


(define (assocify ht)
  (hash-map->list cons ht))

(define (dump)
  (call-with-output-file "hn.msgpack"
    (lambda (port)
      (let loop ((uid (max-id)))
        (display ".")
        (unless (eq? uid 0)
          (let* ((url "https://hacker-news.firebaseio.com/v0/item/~a.json")
                 (url (format #f url uid)))
            (receive (response body) (http-get url)
              (let ((bv (pack (assocify (json-string->scm body)))))
                (put-bytevector port bv))))
          (loop (1- uid)))))))

(define (load)
  (let* ((filename "hn.msgpack")
         (file (open filename  O_RDONLY)))
    (let next-entry ((entry (get-unpack file)))
      (unless (eof-object? entry)
        (pk entry)
        (next-entry (get-unpack file))))))

(dump)
;; (load)
        
