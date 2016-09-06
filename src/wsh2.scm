(define-module (wsh))

(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))

(use-modules (text))


(define (get-or-create-token token)
  (receive (new token) (get-or-create-vertex 'token token)
    (if new
        (save (vertex-set token 'label 'token))
        token)))

(define-public (index document content)
  "Index DOCUMENT vertex based on CONTENT string. CONTENT can plain
   text or html"
  ;; convert CONTENT to tokens
  (let ((tokens (html->tokens content)))
    ;; index the first token of CONTENT
    (let* ((token (get-or-create-token (car tokens)))
           (position (create-vertex '((label . position)))))
      ;; position is an hyperedge
      (create-edge document position '())
      (create-edge position token '())
      (let next ((tokens (cdr tokens))
                 (prev position))
        (unless (null? tokens)
          (let ((token (get-or-create-token (car tokens)))
                (position (create-vertex '((label . position)))))
            (create-edge document position '())
            (create-edge position token '())
            (create-edge prev position '((label . next)))
            (next (cdr tokens) position)))))))

(define (search/token token)
  (let ((uids (ukv-index-ref 'token token)))
    (if (null? uids)
        '()
        (let ((token (get (car uids))))
          (let ((query (gremlin incomings
                                start
                                (where? 'label 'position)
                                incomings
                                start)))
            (stream->list (query token)))))))

(define-public (query/term term)
  (cons 'term term))

(define-public (query/and . args)
  (cons 'and args))

(define-public (query/or . args)
  (cons 'or args))

(define-public (query/not arg)
  (cons 'not arg))

(define (search/vm query)
  (match query
    (('term . term) (search term))
    (('and . args)
     ;; separate 'not' for others
     (let* ((nots (filter (lambda (arg) (eq? (car arg) 'not)) args))
            (others (lset-difference equal? args nots)))
       ;; retrieve hits before applying 'not'
       (let* ((nots (map search/vm nots))
              (hits (apply lset-intersection (cons eq? (map search/vm others)))))
         (let loop ((nots nots)
                    (hits hits))
           (cond
            ((null? nots) hits)
            ((null? hits) '())
            (else (loop (cdr nots) ((car nots) hits))))))))
    (('or . args) (delete-duplicates (append-map search/vm args)))
    (('not . arg) (lambda (hits)
                    (lset-difference eq? hits search/vm)))))

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

(define (term-frequency term-id doc-id)
  "frequency of TERM-ID in DOC-ID"
  (call-with-cursor 'inverted-index
    (lambda (cursor)
      (cursor-count-prefix cursor term-id doc-id 0))))

(define (score term-ids doc-id)
  "score DOC-ID against TERM-IDS"
  (apply + (map (cut term-frequency <> doc-id) term-ids)))

(define-public (search* query)
  "retrieve sorted document ids for QUERY"
  ;; compute hits for query
  (let ((hits (search/vm query)))
    ;; retrieve relevant query terms
    (let ((term-ids (query-terms query)))
      ;; score every hits against terms
      (let ((scores (map (cut score term-ids <>) hits)))
        (let ((urls (map uid->url hits)))
          (sort (map cons urls scores) (lambda (a b) (> (cdr a) (cdr b))) ))))))

;;;
;;; tests
;;;

(use-modules (test-check))

;; (when (or (getenv "CHECK") (getenv "CHECK_WSH"))
;;   (test-check "index"
;;     (with-env (apply env-open* (cons "/tmp/wt" *wsh*))
;;       (not (null? (indeenx "http://example.net" "foo bar baz"))))
;;     #t)

;;   (test-check "query-terms 1"
;;     (with-env (env-open
;;         (index "http://example.net" "database")
;;         (index "http://example.net" "spam")
;;         (index "http://example.net" "egg")
;;         (index "http://example.net" "postgresql")
;;         (index "http://example.net" "pgsql")
;;         (query-terms (search/and (search/term "database") (search/term "spam")))))
;;     '(2 1))

;;   (test-check "query-terms 2"
;;     (receive (cnx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database")
;;         (index "http://example.net" "spam")
;;         (index "http://example.net" "egg")
;;         (index "http://example.net" "postgresql")
;;         (index "http://example.net" "pgsql")
;;         (query-terms (search/and (search/term "database") (search/term "spam")
;;                                      (search/or (search/term "pgsql") (search/term "postgresql"))))))
;;     '(4 5 2 1))

;;   (test-check "query-terms 3"
;;     (receive (cnx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database")
;;         (index "http://example.net" "spam")
;;         (index "http://example.net" "egg")
;;         (index "http://example.net" "postgresql")
;;         (index "http://example.net" "pgsql")
;;         (query-terms (search/and (search/term "database") (search/term "spam")
;;                                      (search/or (search/term "pgsql") (search/term "postgresql"))
;;                                      (search/not (search/term "spam"))))))
;;     '(4 5 2 1))

;;   (test-check "search/vm and/or"
;;     (receive (cnx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & pgsql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql")
;;         (search/vm (search/and (search/term "database") (search/or (search/term "postgresql")
;;                                                                        (search/term "pgsql"))))))
;;     '(5 1))

;;   (test-check "search/vm or avoid duplicates"
;;     (receive (cnx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database")
;;         (index "http://example.net" "wiredtiger & database")
;;         (index "http://example.net" "wiredtiger")

;;         (search/vm (search/or (search/term "database")
;;                                   (search/term "wiredtiger")))))
;;     '(2 1 3))

;;   (test-check "search/vm avoid duplicates"
;;     (receive (cnx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "wiredtiger & wiredtiger")
;;         (search/vm (search/term "wiredtiger"))))
;;     '(1))

;;   (test-check "search/vm and/not"
;;     (receive (cnx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & pgsql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql")
;;         (search/vm (search/and (search/term "database") (search/not (search/term "egg"))))))
;;     '(5 1))

;;   (test-check "search/vm and/not/and"
;;     (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & spam & egg")
;;         (index "http://example.net" "database & spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & spam")
;;         (search/vm (search/and (search/term "database") (search/not (search/and (search/term "egg")
;;                                                                                     (search/term "spam")))))))
;;     '(5 4 1))

;;   (test-check "search/vm and/not"
;;     (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & pgsql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql")
;;         (search/vm (search/and (search/term "database") (search/not (search/or (search/term "egg")
;;                                                                                    (search/term "pgsql")))))))
;;     '(1))

;;   (test-check "search/make-predicate 1"
;;     (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & pgsql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql & spam")
;;         (let* ((query (search/term "database"))
;;                (predicate (search/make-predicate query)))

;;           (filter predicate (map (cut + 1 <>) (iota 5))))))
;;     '(1 4 5))

;;   (test-check "search/make-predicate 2"
;;     (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & pgsql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql & spam")
;;         (let* ((query (search/and (search/term "database")
;;                                   (search/term "postgresql")))
;;                (predicate (search/make-predicate query)))

;;           (filter predicate (map (cut + 1 <>) (iota 5))))))
;;     '(1))

;;   (test-check "search/make-predicate 3"
;;     (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & pgsql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql & spam")
;;         (let* ((query (search/and (search/term "database")
;;                                   (search/or (search/term "postgresql")
;;                                              (search/term "pgsql"))))
;;                (predicate (search/make-predicate query)))

;;           (filter predicate (map (cut + 1 <>) (iota 5))))))
;;     '(1 5))

;;   (test-check "search*"
;;     (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *wsh*))
;;       (with-cnx cnx
;;         (index "http://example.net" "database & postgresql & pgsql")
;;         (index "http://example.net" "database & postgresql")
;;         (index "http://example.net" "spam & egg")
;;         (index "http://example.net" "database & egg")
;;         (index "http://example.net" "database & pgsql & spam")
;;         (index "http://example.net" "database & postgresql & pgsql & database again")
;;         (let ((query (search/and (search/term "database")
;;                                  (search/or (search/term "postgresql")
;;                                             (search/term "pgsql")))))
;;           (search* query))))
;;     '((6 . 4) (1 . 3) (5 . 2) (2 . 2)))
;;   )
