(use-modules (grf))
(use-modules (htmlprag))
(use-modules (http))
(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (rnrs bytevectors))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))
(use-modules (text))
(use-modules (uav))
(use-modules (web response))


(setlocale LC_ALL "")


;;;
;;; Extract urls from a scm file
;;;
;;; The dump must be generated with urls-step00-fetch.scm
;;;
;;; The dump must be a stream of (cons url response-string) where
;;; response-string is provided by `curl -is url`
;;;

(define-stream (file->scm filename)
  (let ((file (open filename O_RDONLY)))
    (stream-let next-entry ((entry (read file)))
      (if (eof-object? entry)
          (begin (close file)
                 stream-null)
          (stream-cons entry (next-entry (read file)))))))

(define (parse pair)
  (let ((response (call-with-input-string (cdr pair) read-response)))
    (case (response-code response)
      ((200) (case (car (response-content-type response))
               ((text/html) (cons (car pair) (utf8->string (read-response-body response))))
               (else '())))
      ((301) '())
      ((404) '())
      (else '()))))

(define (valid? pair)
  (not (null? pair)))

(define (maybe proc)
  (lambda args
    (catch #true
      (lambda () (with-error-to-file "/dev/null" (lambda () (apply proc args))))
      (lambda _ '()))))

(define (html->tokens* pair)
  (cons (car pair) (html->tokens (cdr pair))))


(define (link-to-token vertex)
  (lambda (token)
    (receive (new token) (vertex-get-or-create 'token 'token `((token . ,token)))
      (edge-add! vertex 'contains token '()))))

(define index (match-lambda
                ((url . tokens)
                 (catch #true
                   (lambda ()
                     (let ((vertex (vertex-add! 'url `((url . ,url)))))
                       (for-each (link-to-token vertex) tokens))
                     (display "."))
                   (lambda _ (display "x"))))))

(define cnx (uav-open* "/tmp/wt"))

(with-cnx cnx
  ((compose (cut stream-for-each index <>)
            (cut stream-filter valid? <>)
            (cut stream-map (maybe html->tokens*) <>)
            (cut stream-filter valid? <>)
            (cut stream-map (maybe parse) <>)
            (cut stream-filter valid? <>))
   (file->scm "data/hn.stories.alpha.scm")))
