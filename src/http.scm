(define-module (http))


(use-modules (web response))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))


;;; wrapping curl command

(define-public (curl url)
  (let* ((port (open-input-pipe (format #f "curl -is ~a" url)))
         (response (read-string port)))
    (close-pipe port)
    response))


(define-public (http-get url)
  ;; Create a Curl handle
    (let* ((response-string (curl url))
           ;; Create a string port from the response
           (response-port (open-input-string response-string))
           ;; Have the (web response) module to parse the response
           (response (read-response response-port))
           (body (utf8->string (read-response-body response))))
      (close response-port)
      ;; Have the (web response) module extract the body from the
      ;; response
      (values response body)))
