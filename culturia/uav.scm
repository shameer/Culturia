(define-module (uav))

(use-modules (ice-9 match))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))


(set! *random-state* (random-state-from-platform))


;;;
;;; UAV tuples space
;;;


(define-public *tuples* '(tuples
                          ((uid . string) (attribute . string))
                          ((value . string))
                          ()))

;; shortcuts

(define-public (uav-open path)
  (define connection (connection-open path "create"))
  (define session (session-open connection))
  (session-create* session *tuples*)
  (session-close session)
  connection)

(define-public (uav-context-open connection)
  (context-open connection *tuples*))


;;; database procedures


(define-public (uav-ref context uid attribute)
  (let* ((cursor (context-ref context 'tuples))
         (value (cursor-value-ref* cursor uid attribute)))
    (if (null? value)
        #nil
        (string->scm (car value)))))


(define-public (uav-ref* context uid)
  (let ((cursor (context-ref context 'tuples)))
  (map (match-lambda (((uid attribute) . (value)) 
                      (cons (string->scm attribute) (string->scm value))))
       (cursor-range cursor uid ""))))


(define (make-uid context)
  (generate-uid (lambda (identifier) (not (null? (uav-ref* context identifier))))))


(define-public (uav-add! context assoc)
  (define uid (make-uid context))
  (define cursor (context-ref context 'tuples))
  (define (add! pair)
    (cursor-insert* cursor (list uid (scm->string (car pair))) (list (scm->string (cdr pair)))))
  (for-each add! assoc)
  uid)


(define-public (uav-del! context uid)
  (for-each (match-lambda ((attribute . value) (cursor-remove* (context-ref context 'tuples) uid (scm->string attribute))))
            (uav-ref* context uid)))


;;;
;;; tests
;;;

;; (define connection (uav-open "/tmp/tp"))
;; (define context (uav-context-open connection))


;; (define uid 
;;   (with-transaction context
;;     (uav-add! context '((name . amirouche) (age . 30)))))

;; (pk (uav-ref* context uid))
;; (pk (uav-del! context uid))
;; (pk (uav-ref* context uid))
