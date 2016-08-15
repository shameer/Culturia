(define-module (hn))

(use-modules (rnrs io ports))
(use-modules (ice-9 receive))
(use-modules (http))
(use-modules (json))
(use-modules (msgpack))


(define (max-id)
  (receive (response body) (http-get "https://hacker-news.firebaseio.com/v0/maxitem.json")
    (string->number body)))


(define (assocify assoc)
  (let ((assoc (cdr assoc)))
    assoc))

(define (dump)
  (call-with-output-file "hn.msgpack"
    (lambda (port)
      (let loop ((uid (max-id)))
        (unless (eq? uid 0)
          (let* ((url "https://hacker-news.firebaseio.com/v0/item/~a.json")
                 (url (format #f url uid)))
            (receive (response body) (http-get url)
              (let ((bv (pack (assocify (call-with-input-string body read-json)))))
                (put-bytevector port bv)))))))))

(define (load)
  (let* ((filename "hn.msgpack")
         (file (open filename  O_RDONLY)))
    (let next-entry ((entry (get-unpack file)))
      (unless (eof-object? entry)
        (pk entry)
        (next-entry (get-unpack file))))))

(load)

        
