(define-module (culturia))

(use-modules (culturia wiredtiger))

(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-26)) ;; cut

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))


;; helper for managing exceptions

(define (make-exception name)
  (gensym (string-append "culturia-" (symbol->string name))))

(define *exception* (make-exception 'exception))

;;;
;;; srfi-99
;;;
;;
;; macro to quickly define immutable records
;;
;;
;; Usage:
;;
;;   (define-record-type <abc> field-one field-two)
;;   (define zzz (make-abc 1 2))
;;   (abc-field-one zzz) ;; => 1
;;

(define-syntax define-record-type*
  (lambda (x)
    (define (%id-name name) (string->symbol (string-drop (string-drop-right (symbol->string name) 1) 1)))
    (define (id-name ctx name)
      (datum->syntax ctx (%id-name (syntax->datum name))))
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ rname field ...)
       (and (identifier? #'rname) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'rname #'make- (id-name #'rname #'rname)))
                     (pred (id-append #'rname (id-name #'rname #'rname) #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f (id-name #'rname #'rname) #'- f))
                                        #'(field ...))))
         #'(define-record-type rname
             (cons field ...)
             pred
             (field getter)
             ...))))))

;; ---


;;;
;;; generate-uid
;;;

;; init random with a random state

(set! *random-state* (random-state-from-platform))

(define-public (randome-name exists?)
  "Generate a random string made up alphanumeric ascii chars that doesn't exists
   according to `exists?`"
  (define (random-id)
    (define CHARS "0123456789AZERTYUIOPQSDFGHJKLMWXCVBN")
    ;; append 8 alphanumeric chars from `CHARS`
    ;; 281 474 976 710 656 possible names
    (let loop ((count 8)
               (id ""))
      (if (eq? count 0)
          id
          (loop (1- count) (format #f "~a~a" id (string-ref CHARS (random 36)))))))

  (let loop ()
    ;; generate a random uid until it find an id that doesn't already exists?
    (let ((id (random-id)))
      (if (exists? id) (loop) id))))

;; ---

(define (string->scm value)
  "serialize VALUE with `read` as scheme objects"
  (with-input-from-string (car (unpack "S" value)) (lambda () (read))))

(define (scm->string value)
  "Write VALUE in a string and return it"
  (pack "S" (with-output-to-string (lambda () (write value)))))

;;

(define-record-type* <culturia>
  connection
  session
  ;; <ego>
  ego
  ego-append  ;; secondary cursor for insert
  ego-name  ;; third cursor for name look of egos
  ;; <fact> cursors
  facts  ;; main cursor all the facts used for direct access via uid
  facts-append  ;; secondary cursor for insert
  deleted ;; cursor over the deleted facts index
  index  ;; main index to explore the hyper culture taking into account vcs branch
  types  ;; cursor to fetch facts of given type
  names  ;; cursor for direct access via (type, name)
  ;; <arrow> cursor
  arrows  ;; cursor for fetching outgoings set
  reversed  ;; cursor for fetching incomings set
  )

(set-record-type-printer! <culturia>
                          (lambda (record port)
                            (format port
                                    "<culturia 0x~x ~s>"
                                    (pointer-address (connection-handle record))
                                    (connection-structure-get-home (connection-handle record)))))

;; ---

(define (%make-culturia path)
  (let ((connection (connection-open path "create"))
        (session (session-open connection))
        (columns (string-append "("
                                "uid,"
                                "culture-path,"
                                "revision,"
                                "ego-path,"
                                "type,"
                                "name,"
                                "data"
                                "deleted"
                                ")")))

    ;; create table to store ego informations
    (session-create session
                    "table:ego"
                    (string-append "key_format=r,"
                                   "value_format=SQQS,"
                                   "columns=(uid,name,parent,revision,comment)"))
    
    ;; this index is useful to retrieve egos by name
    (session-create session "index:ego:names" "columns=(name,uid)")
    
    ;; create a main table to store fact informations
    (session-create session
                    "table:facts"
                    (string-append "key_format=r,"
                                   "value_format=uQuSSSQ,"
                                   "columns="
                                   columns))

    ;; create related indices

    ;; this index is useful to check if a fact is deleted in a given ego
    (session-create session "index:facts:deleted" "columns=(ego_path,deleted,uid)")
    
    ;; this index is useful to retrieve facts for given ego and a given
    ;; culture
    (session-create session "index:facts:index" "columns=(ego-path,uid)")
    (session-create session "index:facts:index" "columns=(culture-path,uid)")

    ;; this index is useful to retrieve a set of facts of given a type
    (session-create session "index:facts:types" "columns=(type,uid)")

    ;; this index is useful to retrieve a <fact> of given type and name
    ;; XXX: (type, name) must be unique
    (session-create session "index:facts:names" "columns=(type,name,uid)")


    ;; create a main table to store <arrow>
    (session-create session
                    "table:arrows"
                    (string-append "key_format=uuQQQ,"
                                   "value_format=u,"
                                   "columns=(ego_path,culture_path,revision,start,end)"))
    ;; this index is useful to traverse arrows in reverse ie. retrieve incoming set
    (session-create session "index:arrows:reversed" "columns=(ego_path,culture_path,revision,end,start)")

    (make-culturia connection
                   session
                   ;; <ego>
                   (cursor-open session "table:ego")
                   (cursor-open session "table:ego" "append")
                   ;; <fact> cursors
                   (cursor-open session "table:facts")
                   (cursor-open session "table:facts" "append")
                   (cursor-open session "index:facts:deleted")
                   (cursor-open session "index:facts:index")
                   (cursor-open session "index:facts:types")
                   (cursor-open session "index:facts:names")
                   ;; <arrow> cursor
                   (cursor-open session "index:facts:arrows")
                   (cursor-open session "index:facts:reversed"))))


;; FIXME: add path check and create the initial ego
(define (create-culturia path)
  (%make-culturia path))

(define (open-culturia path)
  (%make-culturia path))

;; ---

(define-public (culturia-begin culturia)
  (session-transaction-begin (culturia-session culturia)))

(define-public (culturia-commit culturia)
  (session-transaction-commit (culturia-session culturia)))

(define-public (culturia-rollback culturia)
  (session-transaction-rollback (culturia-session culturia)))

(define-syntax-rule (with-transaction culturia e ...)
  (begin
    (culturia-begin culturia)
    e ...
    (culturia-commit culturia)))

(export with-transaction)

;; ---

;;; An <ego> is the equivalent of workspace in git
;;
;; An ego is associated with a name, a branch and a revision.
;; ego informations are stored in the `bigbang` culture
;; extra information is stored in wiredtiger to speed up
;; querying.
;;
;; In user code, using an <ego> is the only way to access a <culture>.
;;


(define-record-type* <ego> culturia uid name parent revision)


(define (checkout-ego culturia name #:optional (revision #nil))
  (when revision
    (throw *exception* "wanna play? this is not supported yet!"))
  (let ((cursor (culturia-ego-name culturia)))
    (cursor-set-key cursor name 0)
    (when (eq? (cursor-searh cursor) WT_NOTFOUND)
      (throw *exception* (format #f "~a has no ego named ~s" name revision)))
    (let ((key (cursor-value-ref cursor))
          (value (cursor-value-ref cursor)))
      (match (list key value)
        (((name uid) (name revision parent))
         (make-ego culturia uid name parent revision))
        (_ (throw *exception* "I swear I don't know what I'm doing"))))))


(define (ego-branch name parent comment)
  (let ((cursor (culturia-ego-append culturia))
        (parent (ego-uid parent)))
    (cursor-set-value cursor name parent revision comment)
    (cursor-insert cursor)
    (make-ego culturia (cursor-key-ref cursor) name parent revision)))


(define (ego-commit ego comment)
  (ego-branch "%commit%" ego comment))


(define (ego-fact-ref/uid ego uid))


(define (ego-fact-ref/type+name ego type name))


(define (ego-culture-ref ego name)
  ;; FIXME: check that the culture exists
  (make-culture ego name))


;; ---

;;; A <culture> is a set of interconnected <fact>
;;
;; FIXME: how is related to ego
;;

(define-record-type* <culture> ego name)

(define (create-culture ego name))

(define (culture-delete)
  ;; delete all the facts 

;; ---

(define-record-type <fact>
  (make-fact culture uid type name data outgoings incomings)
  (culture fact-culture)
  (uid fact-uid)
  (type fact-type)
  (name fact-name)
  (data fact-data-ref fact-data-set)
  (outgoings fact-outgoings-ref fact-outgoings-set)
  (incomings fact-incomings-set fact-incomings-set))


(define (create-fact culture type name data)
  (let* ((ego (culture-ego culture))
         (cursor (culturia-facts-append (ego-culturia ego))))
    (cursor-set-value (culture-path culture)
                      (ego-revision ego)
                      (ego-path-suffix ego)
                      type
                      name
                      (scm->string data)
                      #false)
    (cursor-insert cursor)
    (make-fact culture (cursor-key-ref cursor) type name data '() '())))


(define (fact-delete culture fact)
  ;; mark the fact as deleted for the current ego (revision+branch)
  (let* ((ego (culture-ego culture))
         (cursor (culturia-facts (ego-culturia ego))))
    (cursor-set-key cursor (fact-uid fact))
    (cursor-set-value (culture-path culture)
                      (ego-revision ego)
                      (ego-path-suffix ego)
                      (fact-type type)
                      (fact-name name)
                      (scm->string (fact-data fact))
                      #true)
    (cursor-update cursor)))

  
;; ---

(define-record-type* <culture> culturia uid name)
