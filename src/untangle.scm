(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-26)) ;; for cut


(define punctuation (string->list "!\"#$%&\\'()*+,-./:;<=>?@[\\]^_`{|}~"))

(define (clean text)
  "Replace punctuation characters from TEXT with a space character"
  (string-map (lambda (char) (if (list-index punctuation char) #\space char)) text))

(define (clean-newline text)
  (string-map (lambda (char) (if (eq? #\newline char) #\space char)) text))

(define split (cut string-split <> #\space))

(define (sanitize words)
  "Only keep words that have length bigger than one"
  (filter (lambda (word) (< 3 (string-length word))) words))

;; compose must be read from right to left
(define string->tokens (compose sanitize split string-downcase clean-newline clean))

(define (html2text file)
  (let* ((port (open-input-pipe (format #f "html2text ~s" file)))
         (str (read-string port)))
    (close-pipe port)
    str))

(map pk (string->tokens (html2text "Open_Mind_Common_Sense")))
