(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (graphitisay))
(use-modules (ice-9 receive))
(use-modules (srfi srfi-41))


(define priority-queue '(priority-queue
                         ((rating . integer)
                          (name . string)
                          (created-at . positive-integer)
                          (description . string))
                         ((assoc . string))
                         ()))
                         

(define *ipd* (cons* priority-queue *graphitisay*))


(define (priority-queue-save context rating name description assoc)
  (call-with-cursor context 'priority-queue
    (lambda (cursor)
      (cursor-key-set cursor rating name (current-time) description)
      (cursor-value-set cursor (scm->string assoc))
      (cursor-insert cursor))))
    

(define-stream (priority-queue context)
  (let ((cursor (context-ref context 'priority-queue)))
    (stream-let loop ((next? (cursor-next* cursor)))
      (if next?
          (stream-cons (cons (cursor-key-ref cursor)
                             (cursor-value-ref cursor))
                       (loop (cursor-next* cursor)))
          (begin
            (cursor-reset cursor)
            stream-null)))))

;;;
;;; tests
;;;

(use-modules (test-check))

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

  (test-check "priority-queue-save"
    (receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt" *ipd*))
      (with-cnx cnx
        (priority-queue-save ctx 1 "test-1" "héllo world" '())
        (priority-queue-save ctx 10 "test-4" "héllo world" '())
        (priority-queue-save ctx 0 "test-0" "héllo world" '())
        (priority-queue-save ctx 1 "test-2" "héllo world" '())
        (priority-queue-save ctx 2 "test-3" "héllo world" '())
        (map cadar (stream->list (priority-queue ctx)))))

    '("test-0" "test-1" "test-2" "test-3" "test-4")))
 
