(define-module (culture))


(use-modules (srfi srfi-1))

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


(define-public (create-atom #:optional (assoc '()))
  (make-atom #nil assoc))


(define-public (atom-set atom key value)
  (let* ((assoc (atom-assoc atom))
         (assoc (alist-delete assoc key))
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


(define-public (atom-link! atom other)
  (let ((cursor (context-ref context 'links)))
    (cursor-insert* cursor
                    (list (atom-uid atom) (atom-uid other))
                    (list ""))))


(define-public (atom-incomings atom)
  (let ((cursor (context-ref context 'links-incomings)))
    (map cadr (cursor-range* cursor (atom-uid atom)))))


(define-public (atom-outgoings atom)
  (let ((cursor (context-ref context 'links-outgoings)))
    (map cadr (cursor-range* cursor (atom-uid atom)))))


(define-public (atom-unlink atom other)
  (let ((cursor (context-ref context 'links)))
    (cursor-remove* cursor (atom-uid atom) (atom-uid other))))


(define-public (atom-delete atom)
  (let ((cursor (context-ref context 'atoms-append)))
    ;; remove assoc
    (cursor-remove* cursor (atom-uid atom))
    ;; remove links
    (for-each (lambda (uid) (atom-unlink atom (make-atom uid #nil))) (atom-outgoings atom))
    (for-each (lambda (uid) (atom-unlink (make-atom uid #nil) atom)) (atom-incomings atom))))

