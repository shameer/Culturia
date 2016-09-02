(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26))
(use-modules (rnrs io ports))
(use-modules (ice-9 receive))
(use-modules (http))
(use-modules (json))


;; (setlocale LC_ALL "")


;;;
;;; Extract interesting urls from an HN
;;;
;;; The dump must be generated with hn-step-00-dump-hn-api.scm
;;;
;;; The dump must be a scm stream of response string as provided
;;; by curl -is over the HN API. cf. https://github.com/HackerNews/API
;;;

(define (iterator:scm->json filename)
  (let ((file (open filename O_RDONLY)))
    (let loop ()
      (lambda ()
        (let ((entry (read file)))
          (if (eof-object? entry)
              (begin (close file)
                     (values '() '()))
              (values entry (loop))))))))

(define (maybe-json string)
  (catch #true
    (lambda () (call-with-input-string string read-json))
    (lambda _ (display "j") '())))

(define (maybe-body string)
  (catch #true
    (lambda () (response-string->body string))
    (lambda _ (display "b") "")))

(define response-string->json (compose maybe-json maybe-body))

(define (story? item)
  (and (equal? (assoc-ref item "type") "story")
       (assoc-ref item "url")
       (not (equal? (assoc-ref item "url") ""))
       (not (assoc-ref item "deleted"))
       (< 3 (assoc-ref item "score"))))

(define* ((append-to filename) url)
  (display ".")
  (let ((port (open-file filename "a")))
    (put-string port url)
    (put-char port #\newline)
    (close port)))
            
(define (extract-urls input output)
  (let ((generator (iterator:scm->json input))
        (append (append-to output)))
    (let loop ()
      (receive (entry next) (generator)
        (unless (null? entry)
          (let ((json (response-string->json entry)))
            (when (story? json)
              (append (assoc-ref json "url")))
            (loop)))))))
  
(extract-urls "/home/amirouche/data/hn/hn.scm" "/home/amirouche/data/hn/hn.urls.txt")
