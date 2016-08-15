(define-module (http))


(use-modules (curl)
             (web response)
             (ice-9 pretty-print)
             (rnrs bytevectors))


(define-public (http-get url)
  ;; Create a Curl handle
  (let ((handle (curl-easy-init)))
    ;; Set the URL from which to get the data
    (curl-easy-setopt handle 'url url)
    ;; Request that the HTTP headers be included in the response
    (curl-easy-setopt handle 'header #t)
    ;; Get the result as a Latin-1 string
    (let* ((response-string (curl-easy-perform handle))
           ;; Create a string port from the response
           (response-port (open-input-string response-string))
           ;; Have the (web response) module to parse the response
           (response (read-response response-port)))
      ;; Have the (web response) module extract the body from the
      ;; response
      (values response (utf8->string (read-response-body response))))))


