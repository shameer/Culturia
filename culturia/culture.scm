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


(define *index* '(index ((key . string))
                        ((value . string))
                        ()))

(define *trigrams* '(trigrams ((key . record))
                              ((word . string)
                               (trigram . string)
                               (value . string))
                              ((index (trigram) (word value))
                               (value (value) (key)))))


(define-public *culture* (list *atoms* *links* *index* *trigrams*))


;;;
;;; <atom> procedures
;;;

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


(define-public (atom-unlink! atom other context)
  (let ((cursor (context-ref context 'links)))
    (cursor-remove* cursor (atom-uid atom) (atom-uid other))))


(define-public (atom-remove! atom context)
  (let ((cursor (context-ref context 'atoms-append)))
    ;; remove assoc
    (cursor-remove* cursor (atom-uid atom))
    ;; remove links
    (for-each (lambda (uid) (atom-unlink! atom (make-atom uid #nil) context))
              (atom-outgoings atom context))
    (for-each (lambda (uid) (atom-unlink! (make-atom uid #nil) atom context))
              (atom-incomings atom context))))

;;;
;;; *index*
;;;

(define-public (index-set! key value context)
  (let ((cursor (context-ref context 'index)))
    (cursor-insert* cursor (list key) (list (scm->string value)))))

(define-public (index-ref key context)
  (let ((cursor (context-ref context 'index)))
    (if (cursor-search* cursor key)
        (string->scm (car (cursor-value-ref cursor)))
        #nil)))

;;
;; XXX: it's possible to have multiple row with the same value so this
;;      procedure is not enough to remove a value from the index
;;
;; (define-public (index-remove! value context)
;;   (let ((cursor (context-ref context 'index-value))
;;         (key (car (cursor-search* cursor (scm->string value)))))
;;     (let ((cursor (contex-ref context 'index)))
;;       (cursor-remove* cursor key))))
;;

;;; fuzzy index

(define (word->trigrams word)
  (define (word->grams word)
    (let loop ((word word)
               (grams '()))
      (if (>= (string-length word) 3)
          (loop (string-drop word 1) (cons (string-take word 3) grams))
          ;; do not index grams < 3
          (reverse grams))))
  (append-map word->grams (list word (string-take word 1) (string-take word 2))))


(define (levenshtein s t)
  (define (%levenshtein s sl t tl)
    (cond ((zero? sl) tl)
          ((zero? tl) sl)
          (else
	    (min (+ (%levenshtein (cdr s) (- sl 1) t tl) 1)
                 (+ (%levenshtein s sl (cdr t) (- tl 1)) 1)
                 (+ (%levenshtein (cdr s) (- sl 1) (cdr t) (- tl 1))
		    (if (char=? (car s) (car t)) 0 1))))))
  (%levenshtein (string->list s)
		(string-length s)		
		(string->list t)
		(string-length t)))


(define-public (fuzzy-index! word value context)
  (define (index! word value cursor)
    (let ((value (scm->string value)))
      (lambda (trigram)
        (cursor-insert* cursor #nil (list word trigram value)))))

  (let ((cursor (context-ref context 'trigrams-append)))
    (for-each (index! word value cursor) (word->trigrams word))))


(define-public (fuzzy-search word context)
  (define (lookup cursor)
    (lambda (trigram)
      ( map cdr (cursor-range cursor trigram))))

  (define (count counter)
    (lambda (tuple)
      (hashtable-set! counter tuple (+ 1 (hashtable-ref counter tuple 0)))))

  (define (search word)
    (let* ((cursor (context-ref context 'trigrams-index))
           (counter (make-hashtable (cut hash <> 1024) equal?))
           (results (append-map (lookup cursor) (word->trigrams word))))
      (for-each (count counter) results)
      counter))

  (define (counter->ordered-list counter)
    (define (less a b)
      (> (hashtable-ref counter a 0) (hashtable-ref counter b 0)))

    (let ((words (hashtable-keys counter)))
      (sort words less)))

  (define search* (compose vector->list counter->ordered-list search))

  (define (list-head* lst size)
    (if (< (length lst) size)
        lst
        (list-head lst size)))
  
  ;; (search* word))
  (define (less a b)
    (< (levenshtein (car a) word)
       (levenshtein (car b) word)))
  
  (let ((top (list-head* (search* word) 10)))
    (map (compose string->scm cadr) (sort top less))))



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
                                   (atom-remove! other context)
                                   (atom-outgoings atom context))
                                 (list))
                     (connection-close connection)))


  ;;; index

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "index-set! and index-ref"
                                 (begin (index-set! "key" "value" context)
                                        (index-ref "key" context))
                                 "value")
                     (connection-close connection)))

  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "index-ref finds nothing"
                                 (index-ref "key" context)
                                 #nil)
                     (connection-close connection)))

  ;;; trigrams
  (test-check "word->trigrams"
              (word->trigrams "abcdef")
              '("abc" "bcd" "cde" "def"))


  (with-directory
   "/tmp/culturia" (let* ((connection (connection-open "/tmp/culturia" "create"))
                          (_ (apply session-create*  (cons (session-open connection) *culture*)))
                          (context (apply context-open (cons connection *culture*))))
                     (test-check "fuzzy index"
                                 (begin (fuzzy-index! "fuzz" "another" context)
                                        (fuzzy-index! "fuzzing" "other" context)
                                        (fuzzy-index! "fuzzy" "first" context)

                                        (fuzzy-search "fuzzy" context))
                                 '("first" "another" "other"))
                     (connection-close connection)))
  )
