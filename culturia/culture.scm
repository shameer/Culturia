(define-module (culture))

(use-modules (rnrs hashtables))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(use-modules (ice-9 optargs))

(use-modules (plain))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))


;;; database table definition


(define *atoms* '(atoms ((uid . record))
                        ((assoc . string))
                        ()))


(define *links* '(links ((start . unsigned-integer)
                         (end . unsigned-integer))
                        ((value . string))
                        ((outgoings (start) (end))
                         (incomings (end) (start)))))


(define *type+name* '(type+name ((type . string)
                                 (name . string))
                                ((uid . unsigned-integer))
                                ()))


(define *trigrams* '(trigrams ((key . record))
                              ((uid . unsigned-integer)
                               (trigram . string)
                               (word . string))
                              ((index (trigram) (uid word))
                               (reverse (uid) (key)))))


(define-public *culture* (list *atoms* *links* *type+name* *trigrams*))


;;; <atom> procedures


(define-record-type* <atom> uid assoc)


(export atom-uid atom-assoc)


(define*-public (create-atom #:optional (assoc '()))
  (make-atom #nil assoc))


(define-public (atom-set atom key value)
  (let* ((assoc (atom-assoc atom))
         (assoc (alist-delete key assoc))
         (assoc (acons key value assoc)))
    (make-atom (atom-uid atom) assoc)))


(define-public (atom-ref atom key)
  (assoc-ref (atom-assoc atom) key))

         
(define-public (atom-insert! atom context)
  (let ((cursor (context-ref context 'atoms-append)))
    (cursor-insert* cursor
                    #nil
                    (list (scm->string (atom-assoc atom))))
    (set-field atom (atom-uid) (car (cursor-key-ref cursor)))))


(define-public (atom-update! atom context)
  (let ((cursor (context-ref context 'atoms)))
    (cursor-update* cursor
                    (list (atom-uid atom))
                    (list (scm->string (atom-assoc atom))))))


(define-public (atom-ref* uid context)
  (let ((cursor (context-ref context 'atoms)))
    (make-atom uid (string->scm (car (cursor-value-ref* cursor uid))))))


(define-public (atom-link! atom other context)
  (let ((cursor (context-ref context 'links)))
    (cursor-insert* cursor
                    (list (atom-uid atom) (atom-uid other))
                    (list ""))))


(define-public (atom-incomings atom context)
  (let ((cursor (context-ref context 'links-incomings)))
    (map cadr (cursor-range cursor (atom-uid atom)))))


(define-public (atom-outgoings atom context)
  (let ((cursor (context-ref context 'links-outgoings)))
    (map cadr (cursor-range cursor (atom-uid atom)))))


(define-public (atom-unlink atom other context)
  (let ((cursor (context-ref context 'links)))
    (cursor-remove* cursor (atom-uid atom) (atom-uid other))))


(define-public (atom-delete! atom context)
  (let ((cursor (context-ref context 'atoms-append)))
    ;; remove assoc
    (cursor-remove* cursor (atom-uid atom))
    ;; remove links
    (for-each (lambda (uid) (atom-unlink atom (make-atom uid #nil) context))
              (atom-outgoings atom context))
    (for-each (lambda (uid) (atom-unlink (make-atom uid #nil) atom context))
              (atom-incomings atom context))))

;;;

(define-public (atom-type+name-index! atom context)
  (let ((cursor (context-ref context 'type+name))
        (type (atom-ref atom 'type))
        (name (atom-ref atom 'name)))
    (if (and type name)
        (cursor-insert* cursor (list type name) (list (atom-uid atom)))
        #false)))

(define-public (atom-type-search type context)
  (let ((cursor (context-ref context 'type+name)))
    (map cadr (cursor-range cursor type ""))))

(define-public (atom-type+name-search type name context)
  (let ((cursor (context-ref context 'type+name)))
    (map cadr (cursor-range cursor type name))))


;;; trigrams

(define (word->trigrams word)
  (define (word->grams word)
    (let loop ((word word)
               (grams '()))
      (if (>= (string-length word) 3)
          (loop (string-drop word 1) (cons (string-take word 3) grams))
          ;; do not index grams < 3
          (reverse grams))))
  (append-map word->grams (list word (string-take word 1) (string-take word 2))))


(define-public (atom-trigrams-index! atom word context)
  (let ((cursor (context-ref context 'trigrams-append)))
    (for-each (lambda (trigram) (cursor-insert* cursor #nil (list (atom-uid atom) trigram word)))
              (word->trigrams word))))


(define-public (atom-trigrams-search word context)
  (define (lookup cursor)
    (lambda (trigram)
      (map cdr (cursor-range cursor trigram))))

  (define (count counter)
    (lambda (tuple)
      (hashtable-set! counter tuple (+ 1 (hashtable-ref counter tuple 0)))))
  
  (let* ((cursor (context-ref context 'trigrams-index))
         (counter (make-hashtable (cut hash <> 1024) equal?))
         (results (append-map (lookup cursor) (word->trigrams word))))
    (for-each (count counter) results)
    counter))


;;;
;;; tests
;;;


(use-modules (tools))  ;; test-check
(use-modules (path))  ;; with-directory


(when (or (getenv "CHECK") (getenv "CHECK_CULTURE"))

  ;;; atoms
  
  (test-check "atom set"
              (atom-ref (atom-set (create-atom '((a . b))) 'a 'c) 'a)
              'c)

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "open database"
                                 (and #true)
                                 #true)
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "create and retrieve"
                                 (atom-uid (atom-insert! (create-atom) context))
                                 1)
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "create, update and retrieve"
                                 (let* ((atom (atom-insert! (create-atom) context))
                                        (_ (atom-update! (atom-set atom 'a 'b) context)))
                                   (atom-ref (atom-ref* (atom-uid atom) context) 'a))
                                 'b)
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "link outgoings atom"
                                 (let* ((atom (atom-insert! (create-atom) context))
                                        (other (atom-insert! (create-atom) context)))
                                   (atom-link! atom other context)
                                   (atom-outgoings atom context))
                                 (list 2))
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "link outgoings other"
                                 (let* ((atom (atom-insert! (create-atom) context))
                                        (other (atom-insert! (create-atom) context)))
                                   (atom-link! atom other context)
                                   (atom-outgoings other context))
                                 (list))
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "link incomings other"
                                 (let* ((atom (atom-insert! (create-atom) context))
                                        (other (atom-insert! (create-atom) context)))
                                   (atom-link! atom other context)
                                   (atom-incomings other context))
                                 (list 1))
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "link incomings atom"
                                 (let* ((atom (atom-insert! (create-atom) context))
                                        (other (atom-insert! (create-atom) context)))
                                   (atom-link! atom other context)
                                   (atom-incomings atom context))
                                 (list))
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "other atom delete"
                                 (let* ((atom (atom-insert! (create-atom) context))
                                        (other (atom-insert! (create-atom) context)))
                                   (atom-link! atom other context)
                                   (atom-delete! other context)
                                   (atom-outgoings atom context))
                                 (list))
                     (connection-close connection)))
  
  ;;; type+name index

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "type+name search"
                                 (let* ((atom (create-atom '((type . "type") (name . "name"))))
                                        (atom (atom-insert! atom context))
                                        (other (create-atom '((type . "type2") (name . "name2"))))
                                        (other (atom-insert! other context)))
                                   (atom-type+name-index! atom context)
                                   (atom-type+name-index! other context)
                                   (atom-type+name-search "type" "name" context))
                                 (list 1))
                     (connection-close connection)))


  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "type search"
                                 (let* ((atom (create-atom '((type . "type") (name . "name"))))
                                        (atom (atom-insert! atom context)))
                                   (atom-type+name-index! atom context)
                                   (atom-type-search "type" context))
                                 (list 1))
                     (connection-close connection)))

  
  ;;; trigrams

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "trigrams search"
                                 (let* ((atom (atom-insert! (create-atom) context)))
                                   (atom-trigrams-index! atom "atom" context)
                                   (hashtable-keys (atom-trigrams-search "atom" context)))
                                 #((1 "atom")))
                     (connection-close connection)))


  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "trigrams near search"
                                 (let* ((atom (atom-insert! (create-atom) context)))
                                   (atom-trigrams-index! atom "atom" context)
                                   (hashtable-keys (atom-trigrams-search "atomic" context)))
                                 #((1 "atom")))
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "trigrams search no result found"
                                 (let* ((atom (atom-insert! (create-atom) context)))
                                   (atom-trigrams-index! atom "atom" context)
                                   (hashtable-keys (atom-trigrams-search "nothing" context)))
                                 #())
                     (connection-close connection)))

  )
