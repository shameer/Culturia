(define-module (wsh))

(use-modules (text))
(use-modules (ice-9 receive))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (wiredtigerz))
(use-modules (wiredtiger))


(define-public *wsh* '((urls
                        ((uid . record))
                        ((url . string))
                        ())
                       (terms
                        ((uid . record))
                        ((string . string))
                        ((inverse (string) (uid))))
                       (inverted-index
                        ((term . unsigned-integer)
                         (url-uid . unsigned-integer)
                         (position . unsigned-integer))
                        ((nothing . bytes))
                        ((positions (url-uid position) (term))))))

(define (index-term-with-position context url-uid)
  (match-lambda
    ((term position)
     (let ((uid ;; get or create term
            (call-with-cursor context 'terms-inverse
              (lambda (cursor)
                (catch 'wiredtiger
                  (lambda () (car (cursor-value-ref* cursor term)))
                  (lambda _ (call-with-cursor context 'terms-append
                              (lambda (cursor)
                                (cursor-insert* cursor '() (list term))))))))))
       (call-with-cursor context 'inverted-index
         (lambda (cursor)
           (cursor-insert* cursor (list uid url-uid position) (list #vu8()))))))))

(define-public (index context url html)
  (call-with-cursor context 'urls-append
    (lambda (cursor)
      ;; insert url and retrieve its uid
      (let ((url-uid (cursor-insert* cursor '() (list url))))
        ;; convert html to tokens and add positions
        (let* ((terms (html->tokens html))
               (terms+positions (zip terms (iota (length terms)))))
          (for-each (index-term-with-position context url-uid) terms+positions))))))


(define (term-uid ctx term)
  (catch 'wiredtiger
    (lambda() (call-with-cursor ctx 'terms-inverse
                (lambda (cursor)
                  (car (cursor-value-ref* cursor term)))))
    (lambda (key . args) #f)
    #f))

(define (search ctx term)
  (let ((uid (term-uid ctx term)))
    (call-with-cursor ctx 'inverted-index
      (lambda (cursor)
        ;; return document-id and position
        (map cadar (cursor-range-prefix cursor uid 0 0))))))

(define (debug ctx)
  (call-with-cursor ctx 'inverted-index
    (lambda (cursor)
      (cursor-debug cursor))))


(define (search/term term)
  (cons 'term term))

(define (search/and . args)
  (cons 'and args))

(define (search/or . args)
  (cons 'or args))


(define (search/vm ctx query)
  (match query
    (('term . term) (pk 'term term (search ctx term)))
    (('and . args) (pk 'and (apply lset-intersection (cons eq? (map (cut search/vm ctx <>) args)))))
    (('or . args) (pk 'or (append-map (cut search/vm ctx <>) args)))))

;;;
;;; tests
;;;

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_WSH"))
  (test-check "index"
    (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
      (with-cnx cnx
        (not (null? (index ctx "http://example.net" "foo bar baz")))))
    #t)

  (test-check "search/vm"
    (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
      (with-cnx cnx
        (index ctx "http://example.net" "database & postgresql ")
        (index ctx "http://example.net" "spam & pgsql")
        (index ctx "http://example.net" "spam & egg")
        (index ctx "http://example.net" "database & egg")
        (index ctx "http://example.net" "database & pgsql")
        (search/vm ctx (search/and (search/term "database") (search/or (search/term "postgresql")
                                                                       (search/term "pgsql"))))))
    '(5 1))
  )
