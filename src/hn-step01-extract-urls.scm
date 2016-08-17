(use-modules (srfi srfi-1))
(use-modules (srfi srfi-41))
(use-modules (srfi srfi-26))
(use-modules (rnrs io ports))
(use-modules (ice-9 receive))
(use-modules (ice-9 threads))
(use-modules (http))
(use-modules (json))
(use-modules (msgpack))


(setlocale LC_ALL "")


;;;
;;; Extract interesting urls from an HN
;;;
;;; The dump must be generated with hn-step-00-dump-hn-api.scm
;;;
;;; The dump must be a msgpack stream of response string as provided
;;; by curl -is over the HN API. cf. https://github.com/HackerNews/API
;;;

(define-stream (msgpack->response-string-stream filename)
  (let ((file (open filename O_RDONLY)))
    (stream-let next-entry ((entry (get-unpack file)))
      (if (eof-object? entry)
          (begin (close file)
                 stream-null)
          (begin (display ".")
                 (stream-cons entry (next-entry (get-unpack file))))))))

(define (assocify ht)
  (hash-map->list cons ht))

(define (maybe-json string)
  (catch 'json-invalid
    (lambda () (assocify (json-string->scm string)))
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
  (display "O")
  (let ((port (open-file filename "a")))
    (put-string port url)
    (put-char port #\newline)
    (close port)))


(define (extract-urls input output)
  ((compose (cut stream-for-each (append-to output) <>)
            (cut stream-map (cut assoc-ref <> "url") <>)
            (cut stream-filter story? <>)
            (cut stream-map response-string->json <>))
   (msgpack->response-string-stream input)))

(extract-urls "data/hn.msgpack" "data/hn.urls.txt")

;; (stream-for-each pk (stream-map response-string->json (msgpack->response-string-stream "data/hn.msgpack")))
