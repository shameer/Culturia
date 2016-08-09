(use-modules (sxml ssax))
(use-modules (ice-9 match))
(use-modules (srfi srfi-26))
(use-modules (ice-9 receive))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))


(setlocale LC_ALL "")


(define (for-each-element-in-file filepath proc)
  (call-with-input-file filepath
    (lambda (port)
      ((ssax:make-parser
        NEW-LEVEL-SEED 
        (lambda (elem-gi attributes namespaces expected-content seed)
          '())
      
        FINISH-ELEMENT
        (lambda (tag attributes namespaces parent-seed seed)
          (proc tag attributes))
      
        CHAR-DATA-HANDLER
        (lambda (string1 string2 seed)
          (if (string-null? string2) (cons string1 seed)
              (cons* string2 string1 seed)))
        
        DOCTYPE
        (lambda (port docname systemid internal-subset? seed)
          (values #f '() '() seed))
        
        UNDECL-ROOT
        (lambda (elem-gi seed)
          (values #f '() '() seed))
        
        PI
        ((*DEFAULT* .
                    (lambda (port pi-tag seed)
                      (cons
                       (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                       seed))))
        )
       port '()))))

(define *post* '(post
                 ((Id . integer))
                 ((AcceptedAnswerId . integer)
                  (AnswerCount . integer)
                  (Body . string)
                  (CommentCount . integer)
                  (CreationDate . string)
                  (FavoriteCount . integer)
                  (LastActivityDate . string)
                  (LastEditDate . string)
                  (LastEditorUserId . integer)
                  (OwnerUserId . integer)
                  (PostTypeId . integer)
                  (Score . integer)
                  (Tags . string)
                  (Title . string)
                  (ViewCount . integer))
                 ((PostTypeId (PostTypeId) (Id)))))


(define (kind->scm kind value)
  (cond
   ((eq? value #f) (if (eq? kind 'integer) 0 ""))
   ((eq? kind 'integer) (string->number value))
   (else value)))

(define (row->list row spec)
  (map (lambda (spec) (kind->scm (cdr spec) (assoc-ref row (car spec)))) spec))  

(define (extract-record row spec)
  (match spec
    ((name key-format value-format _)
     (list (row->list row key-format) (row->list row value-format)))))
  
;; (define row '((Body . "<p>You definitely need some sort of software to filter out the noise. Some of the other answers here can address that.</p>\n\n<p>If you're still doing the recordings and need to stop the cell phone interference, use <a href=\"http://en.wikipedia.org/wiki/Ferrite%5Fbead\" rel=\"nofollow\">ferrite beads</a> near the base of each speaker.</p>\n\n<p>By the way, just in case anyone is curious, these can be used for small desktop speakers, too.</p>\n") (CommentCount . "1") (CreationDate . "2009-07-08T17:15:59.477") (Id . "27") (LastActivityDate . "2009-07-15T06:48:44.300") (OwnerDisplayName . "T Pops") (OwnerUserId . "45675") (ParentId . "24") (PostTypeId . "2") (Score . "0")))

;; (pk (extract-record row *posts*))

(receive (cnx ctx) (wiredtiger-open* "/tmp/wt" *post*)
  (let ((cursor (context-ref ctx 'post)))
    (for-each-element-in-file "../superuser/dump/Posts.xml"
      (lambda (tag row)
        (when (eq? tag 'row)
          (apply cursor-insert* (cons cursor (extract-record row *post*)))))))
  (connection-close cnx))

