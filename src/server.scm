(define-module (server))

(use-modules (ice-9 threads))
(use-modules (wiredtiger) (wiredtigerz) (grf))

(define mutex (make-mutex))


(define (make-thread counter socket ctx)
  (lambda ()
    (with-mutex mutex
      (format #t "** Running thread ~a\n" counter))
    (while #true
      (let* ((client (car (accept socket)))
             (input (read client))
             (proc (eval input (current-module)))
             ;; FIXME: add error handling
             (out (proc ctx)))
        (write out client)
        (close client)))))

(define (make-thread-cleanup counter ctx)
  (lambda ()
    (with-mutex mutex
      (format #t "** Thread ~a is quitting\n" counter))
    (session-close (context-session ctx))))


(define (make-server-socket port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock (make-socket-address AF_INET (inet-pton AF_INET "127.0.0.1") port))
    (listen sock 128)
    sock))

;;; main

(format #t "* Running server on port 9999\n")

(let* ((running? #t)
       (threads '())
       (socket (make-server-socket 9999))
       (cnx (connection-open "/tmp/wt" "create"))
       (session (session-open cnx)))
  
  ;; install signal handling
  (sigaction SIGINT (lambda (arg)
                      (display "** Quitting...\n")
                      (for-each cancel-thread threads)
                      (for-each join-thread threads)
                      (close socket)
                      (display "* The end!\n")
                      (set! running? #false)))

  ;; init database
  (apply session-create* (cons session *grf*))
  (session-close session)

  (let loop ((counter 3))
    (unless (eq? counter 0)
      (let* ((ctx (apply context-open (cons cnx *grf*)))
             (thread (call-with-new-thread (make-thread counter socket ctx))))
        (set-thread-cleanup! thread (make-thread-cleanup counter ctx))
        (set! threads (cons thread threads))
        (loop (1- counter)))))

  (while running?
    (sleep 10)))
