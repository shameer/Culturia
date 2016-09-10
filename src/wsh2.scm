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

(define (query-terms% query)
  (match query
    (('token . token) (token-uid token))
    (('and . args) (map query-terms% args))
    (('or . args) (map query-terms% args))
    (('not . arg) '())))

(define (query-terms query)
  "convert QUERY to a list of relevant terms for computing the score"
  (flatten (query-terms% query)))

(define (term-frequency term doc)
  "frequency of TERM-ID in DOC-ID"
  (throw 'not-implemented-error))

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

  )
