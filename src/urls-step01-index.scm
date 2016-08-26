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
(use-modules (wiredtigerz))
(use-modules (wsh))


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
               ((text/html)
                (cons (car pair) (utf8->string (read-response-body response))))
               (else '())))
      ((301) (display "r") '())
      ((302) (display "r") '())
      ((303) (display "r") '())
      ((404) (display "n") '())
      (else (display "x") '()))))

(define (valid? pair)
  (not (null? pair)))

(define (maybe proc)
  (lambda args
    (catch #true
      (lambda () (with-error-to-file "/dev/null" (lambda () (apply proc args))))
      (lambda _ '()))))

(define store
  (match-lambda ((url . html)
                 (index url html)
                 (display "."))))


(define env (env-open "/tmp/wt"))
(for-each (cut env-config-add env <>) *wsh*)
(env-create env)

(with-env env
  ((compose (cut stream-for-each store <>)
            (cut stream-filter valid? <>)
            (cut stream-map (maybe parse) <>)
            (cut stream-filter valid? <>))
   (file->scm "data/hn.stories.scm")))
