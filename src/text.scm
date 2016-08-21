(define-module (text))

(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))


;;;
;;; wrapping html2text
;;;
;;
;; inspired from ice-9 popen
;;

(define open-process (@@ (ice-9 popen) open-process))

(define (html2text string)
  (with-error-to-file "/dev/null"
    (lambda ()
      (call-with-values (lambda () (open-process OPEN_BOTH "html2text"))
        (lambda (read-port write-port pid)
          (display string write-port)
          (close-port write-port)
          (let ((str (read-string read-port)))
            (close-port read-port)
            (waitpid pid)
            str))))))

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
(define string->tokens (compose filter-stopwords sanitize split string-downcase clean))

(define-public html->tokens (compose string->tokens html2text))
