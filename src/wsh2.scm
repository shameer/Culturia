(define-module (wsh))

(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (ukv))
(use-modules (grf3))

(use-modules (text))


(define (get-or-create-token token)
  (receive (new token) (get-or-create-vertex 'token/name token)
    (if new
        (save (vertex-set token 'label 'token))
        token)))

(define-public (index document content)
  "Index DOCUMENT vertex based on CONTENT string. CONTENT can plain
   text or html"
  ;; convert CONTENT to tokens
  (let ((tokens (html->tokens content)))
    (when (null? tokens)
      (throw 'wsh "emtpy document"))
    ;; index the first token of CONTENT
    (let* ((token (get-or-create-token (car tokens)))
           (position (create-vertex '((label . position)))))
      ;; position is an hyperedge
      (create-edge token position '((label . at)))
      (create-edge position document '((label . part-of)))
      ;; index the rest of tokens
      (let next ((tokens (cdr tokens))
                 (prev position))
        (unless (null? tokens)
          (let ((token (get-or-create-token (car tokens)))
                (position (create-vertex '((label . position)))))
            (create-edge token position '((label . at)))
            (create-edge prev position '((label . next)))
            (create-edge position document '((label . part-of)))
            (next (cdr tokens) position)))))))

(define (search/token token)
  "Return uids of vertices that are related to TOKEN"
  (let ((query (compose
                ;; fetch document vertex
                (cut traversi-map end <>)
                (cut traversi-filter (key? 'label 'part-of) <>)
                (cut traversi-scatter <>)
                (cut traversi-map outgoings <>)
                ;; fetch position vertex
                (cut traversi-map end <>)
                (cut traversi-filter (key? 'label 'at) <>)
                (cut traversi-scatter <>)
                (cut traversi-map outgoings <>))))
    ;; exec query
    (traversi->list (query (from 'token/name token)))))

(define-public (query/token token)
  (cons 'token token))

(define-public (query/and . args)
  (cons 'and args))

(define-public (query/or . args)
  (cons 'or args))

(define-public (query/not arg)
  (cons 'not arg))

(define (search/vm query)
  (match query
    (('token . token) (search/token token))
    (('and . args)
     ;; separate 'not' from the rest
     (let* ((nots (filter (lambda (arg) (equal? (car arg) 'not)) args))
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
  "flatten LST a list made of sublist into a flat list"
  (let loop ((lst lst)
             (out '()))
    (if (list? lst)
        (if (null? lst)
            out
            (if (list? (car lst))
                (loop (cdr lst) (append (flatten (car lst)) out))
                (loop (cdr lst) (cons (car lst) out))))
        lst)))

(define (token-uid token)
  (let ((uid (traversi->list (from 'token/name token))))
    (if (null? uid)
        '()
        (car uid))))

(define (query-tokens% query)
  (match query
    (('token . token) (token-uid token))
    (('and . args) (map query-tokens% args))
    (('or . args) (map query-tokens% args))
    (('not . arg) '())))

(define (query-tokens query)
  "convert QUERY to a list of relevant tokens for computing the score"
  (flatten (query-tokens% query)))

(define (token-count token doc)
  "Count of TOKEN in DOC"
  (let ((query (compose
                (cut traversi-filter (cut equal? token <>) <>)
                ;; fetch tokens
                (cut traversi-filter (key? 'label 'token) <>)
                (cut traversi-map start <>)
                (cut traversi-filter (key? 'label 'at) <>)
                (cut traversi-scatter <>)
                (cut traversi-map incomings <>)
                ;; fetch positions
                (cut traversi-filter (key? 'label 'position) <>)
                (cut traversi-map start <>)
                (cut traversi-filter (key? 'label 'part-of) <>)
                (cut traversi-scatter <>)
                (cut traversi-map incomings <>))))
    (length (traversi->list (query (list->traversi (list doc)))))))

(define (score tokens doc)
  "score DOC against TOKENS"
  (apply + (map (cut token-count <> doc) tokens)))

(define-public (search query)
  "retrieve sorted document ids for QUERY"
  ;; compute hits for query
  (let ((hits (search/vm query)))
    ;; retrieve relevant query tokens
    (let ((tokens (query-tokens query)))
      ;; score every hits against tokens
      (let ((scores (map (cut score tokens <>) hits)))
        (sort (map cons hits scores) (lambda (a b) (> (cdr a) (cdr b))) )))))

;;;
;;; tests
;;;

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_WSH2"))

  (test-check "shallow index test"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new document) (get-or-create-vertex 'doc/id "http://example.net")
        (not (null? (index document "foo bar baz")))))
    #t)

  (test-check "search/token success"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new document) (get-or-create-vertex 'doc/id "http://example.net")
        (index document "wiredtiger database")
        (equal? (search/token "wiredtiger") (list (vertex-uid document)))))
    #t)

  (test-check "search/token fail 0"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new document) (get-or-create-vertex 'doc/id "http://example.net")
        (index document "some database article")
        (null? (search/token "wiredtiger"))))
    #t)

  (test-check "search/token fail 1"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (null? (search/token "wiredtiger")))
    #t)


  (test-check "search/vm search/token"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new document) (get-or-create-vertex 'doc/id "http://example.net")
        (index document "some database article")
        (equal? (search/vm (query/token "database")) (list (vertex-uid document)))))
    #t)

  (test-check "search/vm search/token not found"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new document) (get-or-create-vertex 'doc/id "http://example.net")
        (index document "some database article")
        (search/vm (query/token "wiredtiger"))))
    '())

  (test-check "search/vm search/and"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new document) (get-or-create-vertex 'doc/id "http://example.net")
        (index document "some wiredtiger database article")
        (equal? (search/vm (query/and (query/token "wiredtiger") (query/token "database")))
                (list (vertex-uid document)))))
    #t)
  
  (test-check "search/vm search/not"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (_ a) (get-or-create-vertex 'doc/id "http://example.net")
        (index a "some wiredtiger database article")
        (receive (_ b) (get-or-create-vertex 'doc/id "http://spam.net")
          (index b "some wiredtiger database article spam")
          (equal? (search/vm (query/and (query/token "wiredtiger") (query/token "database")
                                        (query/not (query/token "spam"))))
                  (list (vertex-uid a))))))
    #t)

  (test-check "search/vm search/or"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (_ a) (get-or-create-vertex 'doc/id "http://example.net")
        (index a "some wiredtiger database article")
        (receive (_ b) (get-or-create-vertex 'doc/id "http://spam.net")
          (index b "some postgresql database article")
          (equal? (search/vm (query/or (query/token "wiredtiger") (query/token "postgresql")))
                  (list (vertex-uid a) (vertex-uid b))))))
    #t)

    (test-check "search"
      (with-env (env-open* "/tmp/wt" (list *ukv*))
        (receive (_ a) (get-or-create-vertex 'doc/id "http://example.net")
          (index a "some wiredtiger database article about wiredtiger")
          (receive (_ b) (get-or-create-vertex 'doc/id "http://another-example.net")
            (index b "some postgresql database article")
            (receive (_ c) (get-or-create-vertex 'doc/id "http://spam.net")
              (index c "some spam")
              (equal? (search (query/or (query/token "wiredtiger") (query/token "postgresql")))
                      (list (cons (vertex-uid a) 2) (cons (vertex-uid b) 1)))))))
      #t)

    (test-check "search empty results"
      (with-env (env-open* "/tmp/wt" (list *ukv*))
        (receive (_ a) (get-or-create-vertex 'doc/id "http://example.net")
          (index a "some wiredtiger database article about wiredtiger")
          (receive (_ b) (get-or-create-vertex 'doc/id "http://another-example.net")
            (index b "some postgresql database article")
            (receive (_ c) (get-or-create-vertex 'doc/id "http://spam.net")
              (index c "some spam")
              (search (query/token "unicorn"))))))
      '())
  )
