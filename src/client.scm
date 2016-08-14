(define-module (client))

(define-public (make-client-socket port)
  (let ((socket (socket PF_INET SOCK_STREAM 0)))
    (connect socket AF_INET INADDR_LOOPBACK port)
    socket))

(define-public call
  (lambda (thunk)
    (let ((socket (make-client-socket 9999)))
      (write thunk socket)
      (let ((out (read socket)))
        (close socket)
        out))))
