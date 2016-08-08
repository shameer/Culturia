(define-module (hyper))

(use-modules (ice-9 popen))
(use-modules (ice-9 receive))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-9 gnu))

(use-modules (web uri))
(use-modules (web response))
(use-modules ((sxml xpath) #:renamer (symbol-prefix-proc 'sxml:)))

(use-modules (htmlprag))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (graphitisay))

(use-modules (https-get))


(setlocale LC_ALL "")


;;;
;;; wrapping html2text
;;;
;;
;; inspired from ice-9 popen
;;
(define open-process (@@ (ice-9 popen) open-process))

(define (html2text string)
  (call-with-values (lambda () (open-process OPEN_BOTH "html2text"))
    (lambda (read-port write-port pid)
      (display string write-port)
      (close-port write-port)
      (let ((str (read-string read-port)))
        (close-port read-port)
        (waitpid pid)
        str))))

;;;
;;; tokenizing
;;;

;; english stop words
;; XXX: actually not very useful

(define stopwords (make-hash-table))

(with-input-from-file "stopwords.en.txt"  ;; snarffed from http://www.ranks.nl/stopwords/
  (lambda ()
    (let loop ((stopword (read-line)))
      (unless (eof-object? stopword)
        (hash-set! stopwords stopword #t)
        (loop (read-line))))))

(define (filter-stopwords lst)
  (filter (lambda (token) (not (hash-ref stopwords token))) lst))

;; tokens

(define punctuation (string->list "!\"#$%&\\'()*+,-./:;<=>?@[\\]^_`{|}~"))

(define (clean text)
  "Replace punctuation characters from TEXT with a space character"
  (string-map (lambda (char) (if (list-index (cut equal? char <>) punctuation) #\space char)) text))

(define (clean-newlines text)
  (string-map (lambda (char) (if (eq? #\newline char) #\space char)) text))

(define split (cut string-split <> #\space))

(define (sanitize words)
  "Only keep words that have length bigger than one"
  (filter (lambda (word) (< 1 (string-length word))) words))

;; XXX: compose must be read from right to left
(define string->tokens (compose filter-stopwords sanitize split string-downcase clean-newlines clean))

;;;
;;; extracting links
;;;

(define (extract-href sxml)
  (map cadr ((sxml:sxpath '(// a @ href)) sxml)))

(define (url-domain url)
  (string-take url
               (cond
                ((string-prefix? "http://" url)
                 (let ((has-slash (string-index (string-drop url 7) #\/)))
                   (if has-slash
                       (+ 7 has-slash)
                       (string-length url))))
                ((string-prefix? "https://" url)
                 (let ((has-slash (string-index (string-drop url 8) #\/)))
                   (if has-slash
                       (+ 8 has-slash)
                       (string-length url))))
                (else (string-length url)))))

(define (proprify url)
  "Remove extract / in URL path"
  (let* ((path (string-split (uri-path (string->uri url)) #\/))
         (clean (lambda (string)
                  (not (or (equal? string "") (equal? string ".")))))
         (path (filter clean path))
         (join (lambda (lst) (string-join lst "/")))
         (make-url (lambda (path)
                     (string-append (url-domain url) "/" (join path)))))
    (make-url (let loop ((path path) (out '()))
                (if (null? path)
                    (reverse out)
                    (if (equal? (car path) "..")
                        (loop (cdr path) (cdr out))
                        (loop (cdr path) (cons (car path) out))))))))

(define* ((href->url original-document-url) href)
  (proprify (cond
             ((string-prefix? "http" href) href)
             ((string-prefix? "//" href)
              (if (string-prefix? "http://" original-document-url)
                  (string-append "http:" href)
                  (string-append "https:" href)))
             ((string-prefix? "/" href)
              (string-append (url-domain original-document-url) href))
             ;; ./foo/bar/baz and foo/bar/baz
             (else
              (if (string-suffix? "/" original-document-url)
                  (string-append original-document-url href)
                  (string-append (url-domain original-document-url)
                                 "/"
                                 (dirname (uri-path (string->uri original-document-url)))
                                 "/"
                                 href))))))

(define (unsupported-href href)
  (not (or (string-prefix? "#" href)
           (string-prefix? "mailto:" href))))

(define (extract-links original-document-url string)
  (let ((hrefs (extract-href (html->sxml string))))
    (delete-duplicates
     (map (href->url original-document-url)
          (filter unsupported-href hrefs)))))

(define html->tokens (compose string->tokens html2text))

(define-public (extract url)
  (receive (response body) (http-get* url)
    (case (response-code response)
      ((200) (case (car (response-content-type response))
               ((text/html) (let ((body (read-string body)))
                              (values (extract-links url body) body)))
               (else (values '() '()))))
      ((301) (values (list (uri->string (assoc-ref (response-headers response) 'location)))
                     '()))
      ((404) (values '() '())))))

;;;
;;; helpers
;;;;

(define uri-domain (compose uri-host string->uri))

;;;
;;; database primitives
;;;

(define (make-string-index name)
  `(,name ((key . string))
          ((uid . unsigned-integer))
          ()))

(define-public *hyper* (cons* (make-string-index 'url)
                              (make-string-index 'token)
                              (make-string-index 'domain)
                              *graphitisay*))


(define-public (get-or-create key make-key create)
  (lambda (ctx value)
    (catch 'wiredtiger
      (lambda ()
        (car (cursor-value-ref* (context-ref ctx key) (make-key value))))
      (lambda (key . args)
        (create ctx value)))))

(define-public (create key make-assoc)
  (lambda (ctx value)
    (let* ((assoc (make-assoc value))
           (uid (vertex-add ctx (symbol->string key) assoc)))
      (cursor-insert* (context-ref ctx key) (list (assoc-ref assoc key)) (list uid))
      uid)))

(define domain-create (create 'domain
                              (lambda (url)
                                `((domain . ,(uri-domain url))
                                  (url . ,url)
                                  (added-at . ,(current-time))
                                  (flag . blacklisted)))))

(define-public domain-get-or-create (get-or-create 'domain uri-domain domain-create))

;; (define (domain-whitelist! ctx url)
;;   ;; retrieve domain vertex
;;   (let ((vertex (string->domain url)))
;;     ;; update assoc
;;     (let* ((assoc (vertex-assoc vertex))
;;            (assoc (alist-delete assoc 'flag))
;;            (assoc (acons 'flag 'whitelisted assoc)))
;;       ;; update vertex and save
;;       (let ((new (set-field vertex (vertex-assoc) assoc)))
;;         (vertex-save ctx new)))))

(define-public (domain-index! ctx url)
  (format #t "* indexing domain ~s\n" url)
  (let loop ((urls (list (assoc-ref (vertex-assoc (string->domain-url ctx url)) 'url))))
    (if (null? urls)
        (format #t "* done\n")
        (if (url-indexed? ctx (car urls))
            (loop (cdr urls))
            (let* ((uid (url-index!* ctx (car urls)))
                   (links (url-links ctx (car urls)))
                   (new (filter (lambda (link) (equal? (uri-domain url) (uri-domain link)))
                                links)))
          (loop (lset-union equal? new (cdr urls))))))))

(define url-create# (create 'url (lambda (url)
                                   `((url . ,url)
                                     (added-at . ,(current-time))
                                     (crawled-at . 0)))))

(define (url-create ctx url)
  (let* ((uri (string->uri url))
         (host (uri-host uri))
         (scheme (symbol->string (uri-scheme uri)))
         (domain (string-append scheme "://" host))
         (url-uid (url-create# ctx url))
         (domain-uid (domain-get-or-create ctx domain)))
    (edge-add ctx url-uid "part of" domain-uid '())
    url-uid))

(define-public (urls-count ctx)
  (let ((cursor (context-ref ctx 'token)))
    (with-cursor cursor
      (let loop ((count 0)
                 (next (cursor-next* cursor)))
        (if next
            (loop (1+ count) (cursor-next* cursor))
            count)))))

(define-public url-get-or-create (get-or-create 'url values url-create))

(define-public (url-ref ctx uid)
  "Retrieve url associated with UID"
  (vertex-assoc-ref (vertex-ref ctx uid) 'url))

(define-public (string->domain ctx url)
  (let* ((uri (string->uri url))
         (host (uri-host uri))
         (scheme (symbol->string (uri-scheme uri)))
         (domain (string-append scheme "://" host "/")))
    (vertex-ref ctx (domain-get-or-create ctx domain))))

(define (string->domain-url ctx url)
  (let* ((domain (string->domain ctx url))
         (url (assoc-ref (vertex-assoc domain) 'url)))
    (vertex-ref ctx (url-get-or-create ctx url))))


(define (url-indexed? ctx url)
  (let ((crawled-at (assoc-ref (vertex-assoc (vertex-ref ctx (url-get-or-create ctx url))) 'crawled-at)))
    ;; was it crawled 7 days ago
    (< (- (current-time) (* 3600 24 7)) crawled-at)))


(define (url-indexed! ctx url)
  (let* ((vertex (vertex-ref ctx (url-get-or-create ctx url)))
         (assoc (vertex-assoc vertex))
         (assoc (acons 'crawled-at (current-time) (alist-delete 'crawled-at assoc)))
         (vertex (set-field vertex (vertex-assoc) assoc)))
    (vertex-save ctx vertex)))

(define (url-cache! ctx uid html)
  (vertex-save ctx (vertex-assoc-update (vertex-ref ctx uid) 'cache html)))

(define-public (url-index! ctx url)
  (format #t "* indexing ~s\n" url)
  (receive (links html) (extract url)
    (unless (and (null? links) (null? html))
      (let ((url-uid (url-get-or-create ctx url))
            (tokens (map (cut token-get-or-create ctx <>) (delete-duplicates (html->tokens html)))))
        (url-cache! ctx url-uid html)
        (for-each (cut edge-add ctx <> "part of" url-uid '()) tokens)
        (let ((links-uids (map (cut url-get-or-create ctx <>) links)))
          (for-each (cut edge-add ctx <> "include link to" url-uid '()) links-uids))
        (url-indexed! ctx url)
        url-uid))))

(define-public (url-index!* ctx url)
  (catch #true
    (lambda () (with-transaction ctx (url-index! ctx url)))
    (lambda rest
      (context-rollback ctx)
      (format #true "** failed to index ~s\n" url)
      (format #true "*** ~a\n" rest)
      (url-indexed! ctx url)
      #false)))

(define-public (url-links ctx url)
  (let ((uid (url-get-or-create ctx url)))
    (map (lambda (vertex) (assoc-ref (vertex-assoc vertex) 'url))
         (filter (cut vertex-label? <> "url")
                 (map (cut vertex-ref ctx <>)
                      (map edge-start
                           (filter (cut edge-label? <> "include link to")
                                   (map (cut edge-ref ctx <>) (vertex-incomings ctx uid)))))))))

(define token-create (create 'token (lambda (token)
                              `((token . ,token)
                                (added-at . ,(current-time))))))

(define-public token-get-or-create (get-or-create 'token values token-create))

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
