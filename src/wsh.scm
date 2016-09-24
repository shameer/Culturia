(define-module (wsh))

(use-modules (ice-9 receive))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

(use-modules (wiredtigerz))
(use-modules (wiredtiger))

(use-modules (text))


(define-public *wsh* '((terms
                        ((uid . record))
                        ((string . string))
                        ((inverse (string) (uid))))
                       (inverted-index
                        ((term . unsigned-integer)
                         (document . string)
                         (position . unsigned-integer))
                        ((nothing . bytes))
                        ((positions (document position) (term))))))

(define (index-term-with-position document)
  (match-lambda
    ((term position)
     (let ((uid ;; get or create term
            (call-with-cursor 'terms-inverse
              (lambda (cursor)
                (catch 'wiredtiger
                  (lambda () (car (cursor-value-ref* cursor term)))
                  (lambda _ (call-with-cursor 'terms-append
                              (lambda (cursor)
                                (cursor-insert* cursor '() (list term))))))))))
       (call-with-cursor 'inverted-index
         (lambda (cursor)
           (cursor-insert* cursor (list uid document position) (list #vu8()))))))))

(define-public (index document html)
  ;; convert html to tokens and add positions
  (let* ((terms (html->tokens html))
         (terms+positions (zip terms (iota (length terms)))))
    (for-each (index-term-with-position document) terms+positions)))

(define (uid->url uid)
  (call-with-cursor 'urls
    (lambda (cursor)
      (car (cursor-value-ref* cursor uid)))))

(define (term-uid term)
  (catch 'wiredtiger
    (lambda() (call-with-cursor 'terms-inverse
                (lambda (cursor)
                  (car (cursor-value-ref* cursor term)))))
    (lambda (key . args) '())))

(define (search/term term)
  (let ((uid (term-uid term)))
    (if (null? uid)
        '()
        (call-with-cursor 'inverted-index
          (lambda (cursor)
            ;; return document-id and position
            (delete-duplicates (map cadar (cursor-range-prefix cursor uid "" 0))))))))

(define-public (debug)
  (call-with-cursor 'inverted-index
    (lambda (cursor)
      (cursor-debug cursor))))

(define-public (query/term term)
  (cons 'term term))

(define-public (query/and . args)
  (cons 'and args))

(define-public (query/or . args)
  (cons 'or args))

(define-public (query/not arg)
  (cons 'not arg))

(define (query% token)
  (if (string-prefix? "-" token)
      (query/not (query/term (string-drop token 1)))
      (query/term token)))

(define-public (query string)
  (let ((string (string-downcase string)))
    (let ((tokens (filter (lambda (x) (not (equal? x ""))) (string-split string #\space))))
      (apply query/and (map query% tokens)))))
        
(define (search/vm query)
  (match query
    (('term . term) (search/term term))
    (('and . args)
     ;; separate 'not' for others
     (let* ((nots (filter (lambda (arg) (eq? (car arg) 'not)) args))
            (others (lset-difference equal? args nots)))
       ;; retrieve hits before applying 'not'
       (let* ((nots (map search/vm nots))
              (hits (apply lset-intersection (cons equal? (map search/vm others)))))
         (let loop ((nots nots)
                    (hits hits))
           (cond
            ((null? nots) hits)
            ((null? hits) '())
            (else (loop (cdr nots) ((car nots) hits))))))))
    (('or . args) (delete-duplicates (append-map search/vm args)))
    (('not . arg) (lambda (hits)
                    (lset-difference equal? hits (search/vm arg))))))

(define (flatten lst)
  (let loop ((lst lst)
             (out '()))
    (if (list? lst)
        (if (null? lst)
            out
            (if (list? (car lst))
                (loop (cdr lst) (append (flatten (car lst)) out))
                (loop (cdr lst) (cons (car lst) out))))
        lst)))

(define (query-terms% query)
  (match query
    (('term . term) (term-uid term))
    (('and . args) (map query-terms% args))
    (('or . args) (map query-terms% args))
    (('not . arg) '())))

(define (query-terms query)
  "convert QUERY to a list of relevant terms for computing the score"
  (flatten (query-terms% query)))

(define (term-frequency term-id document)
  "frequency of TERM-ID in DOCUMENT"
  (call-with-cursor 'inverted-index
    (lambda (cursor)
      (cursor-count-prefix cursor term-id document 0))))

(define (score term-ids document)
  "score DOCUMENT against TERM-IDS"
  (apply + (map (cut term-frequency <> document) term-ids)))

(define-public (search* query)
  "retrieve sorted urls for QUERY"
  ;; compute hits for query
  (let ((hits (search/vm query)))
    ;; retrieve relevant query terms
    (let ((term-ids (query-terms query)))
      ;; score every hits against terms
      (let ((scores (map (cut score term-ids <>) hits)))
        (sort (map cons hits scores) (lambda (a b) (> (cdr a) (cdr b))) )))))

;;;
;;; tests
;;;

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_WSH"))
  (format #t "* check wsh\n")
  
  (test-check "index"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (not (null? (index "http://example.net" "foo bar baz"))))
    #t)

  (test-check "query-terms 1"
    (with-env (env-open* "/tmp/wt" *wsh*)
        (index "http://example.net" "database")
        (index "http://example.net" "spam")
        (index "http://example.net" "egg")
        (index "http://example.net" "postgresql")
        (index "http://example.net" "pgsql")
        (query-terms (query/and (query/term "database") (query/term "spam"))))
    '(2 1))

  (test-check "query-terms 2"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "http://example.net" "database")
      (index "http://example.net" "spam")
      (index "http://example.net" "egg")
      (index "http://example.net" "postgresql")
      (index "http://example.net" "pgsql")
      (query-terms (query/and (query/term "database") (query/term "spam")
                               (query/or (query/term "pgsql") (query/term "postgresql")))))
    '(4 5 2 1))

  (test-check "query-terms 3"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "http://example.net" "database")
      (index "http://example.net" "spam")
      (index "http://example.net" "egg")
      (index "http://example.net" "postgresql")
      (index "http://example.net" "pgsql")
      (query-terms (query/and (query/term "database") (query/term "spam")
                               (query/or (query/term "pgsql") (query/term "postgresql"))
                               (query/not (query/term "spam")))))
    '(4 5 2 1))

  (test-check "search/vm and/or"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "database & postgresql")
      (index "2" "spam & pgsql")
      (index "3" "spam & egg")
      (index "4" "database & egg")
      (index "5" "database & pgsql")
      (search/vm (query/and (query/term "database") (query/or (query/term "postgresql")
                                                              (query/term "pgsql")))))
    '("5" "1"))

  (test-check "search/vm or avoid duplicates"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "database")
      (index "2" "wiredtiger & database")
      (index "3" "wiredtiger")
      
      (search/vm (query/or (query/term "database")
                            (query/term "wiredtiger"))))
    '("2" "1" "3"))

  (test-check "search/vm avoid duplicates"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "wiredtiger & wiredtiger")
      (search/vm (query/term "wiredtiger")))
    '("1"))

  (test-check "search/vm and/not"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "database & postgresql")
      (index "2" "spam & pgsql")
      (index "3" "spam & egg")
      (index "4" "database & egg")
      (index "5" "database & pgsql")
      (search/vm (query/and (query/term "database")
                             (query/not (query/term "egg")))))
  '("5" "1"))

  (test-check "search/vm and/not/and"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "database & egg")
      (index "2" "database & spam & egg")
      (index "3" "database & spam & egg")
      (index "4" "database & egg")
      (index "5" "database & spam")
      (search/vm (query/and (query/term "database") (query/not (query/and (query/term "egg")
                                                                              (query/term "spam"))))))
    '("5" "4" "1"))

  (test-check "search/vm and/not"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "database & postgresql")
      (index "2" "spam & pgsql")
      (index "3" "spam & egg")
      (index "4" "database & egg")
      (index "5" "database & pgsql")
      (search/vm (query/and (query/term "database")
                             (query/not (query/or (query/term "egg")
                                                    (query/term "pgsql"))))))
    '("1"))
  
  (test-check "search*"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "http://database.postgresql.pgsql.net" "database & postgresql & pgsql")
      (index "http://database.postgresql.net" "database & postgresql")
      (index "http://spam.egg.net" "spam & egg")
      (index "http://database.egg.net" "database & egg")
      (index "http://database.pgsql.spam.net" "database & pgsql & spam")
      (index "http://database.postgresql.pgsql.net/database" "database & postgresql & pgsql & database again")
      (let ((query (query/and (query/term "database")
                               (query/or (query/term "postgresql")
                                          (query/term "pgsql")))))
        (search* query)))
    '(("http://database.postgresql.pgsql.net/database" . 4)
      ("http://database.postgresql.pgsql.net" . 3)
      ("http://database.postgresql.net" . 2)
      ("http://database.pgsql.spam.net" . 2)))

  (test-check "search/vm unknown keyword"
    (with-env (env-open* "/tmp/wt" *wsh*)
      (index "1" "database & postgresql")
      (index "2" "spam & pgsql")
      (index "3" "spam & egg")
      (index "4" "database & egg")
      (index "5" "database & pgsql")
      (search/vm (query/and (query/term "wiredtiger"))))
    '())

  (test-check "query"
    (query "abc def -xxx")
    '(and (term . "abc") (term . "def") (not . (term . "xxx"))))
)
