(define-module (text))

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(use-modules (htmlprag))
(use-modules (html2text))

;;; backward compatible with old html2text bindings
;;; TODO: use html->text directly

(define-public (html2text string)
  (call-with-output-string
    (lambda (port)
      (html->text (html->sxml string) port))))

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

(define punctuation (string->list "!\"#$%&\\'()*+,-./:;<=>?@[\\]^_`{|}~\n\t"))

(define (clean text)
  "Replace punctuation characters from TEXT with a space character"
  (string-map (lambda (char) (if (list-index (cut equal? char <>) punctuation) #\space char)) text))

(define split (cut string-split <> #\space))

(define (sanitize words)
  "Only keep words that have length bigger than one"
  (filter (lambda (word) (< 1 (string-length word))) words))

;; XXX: compose must be read from right to left
(define string->tokens (compose sanitize split string-downcase clean))

(define-public html->tokens (compose string->tokens html2text))
