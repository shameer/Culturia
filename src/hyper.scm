(define-module (hyper))

;;;
;;; helpers
;;;;

(define uri-domain (compose uri-host string->uri))


;;; public API

(define-public (search ctx token)
  (let ((uid (cursor-value-ref* (context-ref ctx 'token) token)))
    (if uid
        (map edge-end
             (filter (cut edge-label? <> "part of")
                     (map (cut edge-ref ctx <>)
                          (vertex-outgoings ctx (car uid)))))
        '())))


(define (intersection a b)
  (lset-intersection equal? a b))

(define-public (search* ctx token . tokens)
  (let loop ((tokens tokens)
             (out (search ctx token)))
    (if (null? tokens)
        (delete-duplicates out)
        (loop (cdr tokens)
              (intersection (search ctx (car tokens)) out)))))

(define (url-token-frequency ctx uid token)
  (let ((url (vertex-ref ctx uid))
        (tokens (html->tokens (vertex-assoc-ref (vertex-ref ctx uid) 'cache))))
    (count (cut equal? token <>) tokens)))

(define (idf ctx token N)
  (log (/ N (length (vertex-outgoings ctx (token-get-or-create ctx token))))))

(define (tf-idf ctx uid token idf)
  "compute the td-idf score of given token"
  (* (url-token-frequency ctx uid token) idf))

(define-public (search** ctx token . tokens)
  "search and sort results according to TF-IDF score"
  (let* ((results (apply search* (cons* ctx token tokens)))
         (N (urls-count ctx))
         (idfs (map (lambda (token) (cons token (idf ctx token N)))
                    (cons token tokens)))
         (scored (map (lambda (uid)
                        (cons uid
                              (apply + (map (lambda (token)
                                              (tf-idf ctx uid token (assoc-ref idfs token)))
                                            (cons token tokens)))))
                      results)))
    (sort scored (lambda (a b)
                   (< (cdr a) (cdr b))))))




;;; boolean logic search

;; FIXME: avoid to iterate over all urls

(define (make-search-operator operator base)
  (lambda fs
    (lambda (uid)
      (fold operator base (map (lambda (f) (if (procedure? f) (f uid) f)) fs)))))

;; FIXME: check for the correctness of the following
;; in particular I'm not sure whether base is correct
(define-public xor* (make-search-operator logxor 0))
(define-public and* (make-search-operator logand 1))
(define-public or* (make-search-operator logior 1))

(define-public (not* a)
  (lambda (uid)
    (if (procedure? a) (not (a uid)) (not a))))

(define-public (token ctx name)
  (lambda (uid)
    ...))
