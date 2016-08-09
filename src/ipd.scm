(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (graphitisay))
(use-modules (srfi srfi-41))


(define priority-queue '(priority-queue
                         ((rating . integer)
                          (name . string)
                          (created-at . positive-integer)
                          (description . string))
                         ((assoc . string))
                         ()))
                         

(define *ipd* (cons* priority-queue *graphitisay*))


(define (priority-queue-save ctx rating name description assoc)
  (call-with-cursor context 'priority-queue
    (lambda (cursor)
      (cursor-set-key cursor rating name (current-time) description)
      (cursor-set-value cursor (scm->string assoc))
      (cursor-insert! cursor))))
    

(define-stream (priority-queue ctx)
  (call-with-cursor context 'priority-queue
    (lambda (proc) (display "aaa")))
  )

;;;
;;; tests
;;;

;;; test-check

(define (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be prepend"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))

(define (path-dfs-walk dirpath proc)
  (define dir (opendir dirpath))
  (let loop ()
    (let ((entry (readdir dir)))
      (cond
       ((eof-object? entry))
       ((or (equal? entry ".") (equal? entry "..")) (loop))
       (else (let ((path (path-join dirpath entry)))
               (if (equal? (stat:type (stat path)) 'directory)
                   (begin (path-dfs-walk path proc)
                          (proc path))
                   (begin (proc path) (loop))))))))
  (closedir dir)
  (proc (path-join dirpath)))

(define (rmtree path)
  (path-dfs-walk path (lambda (path)
                        (if (equal? (stat:type (stat path)) 'directory)
                            (rmdir path)
                            (delete-file path)))))

(define-syntax-rule (with-directory path e ...)
  (begin
    (when (access? path F_OK)
      (rmtree path))
    (mkdir path)
    e ...
    (rmtree path)))

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (with-directory "/tmp/wt"
                     (format #t "** Checking ~a\n" title)
                     (let* ((expected expected-result)
                            (produced tested-expression))
                       (if (not (equal? expected produced))
                           (begin (format #t "*** Expected: ~s\n" expected)
                                  (format #t "*** Computed: ~s\n" produced))))))))

(define-syntax-rule (with-cnx cnx e ...)
  (let ((out (begin e ...)))
    (connection-close cnx)
    out))

(when (or (getenv "CHECK") (getenv "CHECK_IPD"))
  (format #true "* testing ipd\n")
 
  ;; test wiredtigerz

  (test-check "testing wiredtigerz"
    (receive (cnx ctx)
        (wiredtiger-open* "/tmp/wt"
                          '(table ((key . record)) ((value . integer)) ()))

      (with-cnx cnx #true))
    #true)

  ;; test ipd

  (test-check "wiredtiger-open*"
    (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *ipd*))
      (with-cnx cnx
        (priority-queue-save ctx 1 "test-1" "héllo world" '())
        (priority-queue-save ctx 10 "test-4" "héllo world" '())
        (priority-queue-save ctx 0 "test-0" "héllo world" '())
        (priority-queue-save ctx 1 "test-2" "héllo world" '())
        (priority-queue-save ctx 2 "test-3" "héllo world" '())
        (stream->list (priority-queue ctx))))

    '()))
 
