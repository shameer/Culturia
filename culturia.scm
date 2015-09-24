(define-module (culturia))

(use-modules (culturia wiredtiger))

(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-19)) ;; date
(use-modules (srfi srfi-26)) ;; cut

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))


;; helper for managing exceptions

(define (make-exception name)
  "Generate a unique symbol prefixed with NAME"
  (gensym (string-append "culturia-" name)))

(define *exception* (make-exception "culturia-exception"))

(define (raise message . rest)
  "shorthand to throw EXCEPTION with MESSAGE formated with REST"
  (throw *exception* (apply format (append (list #false message) rest))))

(define (Oops!)
  (raise "Oops!"))



;; ---

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

;;; recursive datastructure


(define-record-type <tree>
  (make-tree value children max)
  tree?
  (value tree-value)
  (children tree-children-ref tree-children-set!)
  (max tree-max-ref tree-max-set!))

(export make-tree)

(define-public (create-tree value)
  (make-tree value '() 0))


(define-public (tree-ref tree value)
  (if (eq? (tree-value tree) value)
      tree
      (let loop ((children (tree-children-ref tree)))
        (if (null? children)
          #nil
          (let ((candidate (tree-ref (car children) value)))
            (if (null? candidate)
                (loop (cdr children))
                candidate))))))


(define-public (tree-append! tree parent value)
  (let ((node (tree-ref tree parent)))
    (when (null? node)
      (raise "tree: no node ~a found in ~a" parent tree))
    (tree-children-set! node
                        (cons (create-tree value) (tree-children-ref node)))
    (when (< (tree-max-ref tree) value)
      (tree-max-set! tree value))))


(define-public (tree-path tree value)
  (define (tree-path-rec tree value)
    (if (eq? (tree-value tree) value)
        (list value)
        (let loop ((children (tree-children-ref tree)))
          (if (null? children)
              #nil
              (let ((candidate (tree-path-rec (car children) value)))
                (if (null? candidate)
                    (loop (cdr children))
                    (cons (tree-value tree) candidate)))))))
  (reverse (tree-path-rec tree value)))


;; ---

;;;
;;; generate-uid
;;;

;; init random with a random state

(set! *random-state* (random-state-from-platform))

(define-public (random-name exists?)
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
  (with-input-from-string value (lambda () (read))))

(define (scm->string value)
  "Write VALUE in a string and return it"
  (with-output-to-string (lambda () (write value))))

;; --

;;; <culturia> is handle over the underlying backing store

(define-record-type* <culturia>
  connection
  session
  ;; <revision>
  revisions
  revisions-append  ;; secondary cursor for insert
  revisions-names  ;;
  revisions-tree
  ;; <culture>
  cultures
  cultures-append  ;; secondary cursor for insert
  cultures-names  ;; third cursor for name look of egos
  ;; <atom> cursors
  atoms  ;; main cursor all the atoms used for direct access via uid
  atoms-append  ;; secondary cursor for insert
  atoms-revisions
  atoms-cultures
  atoms-names
  atoms-type-names
  ;; <arrow> cursor
  arrows
  arrows-append
  arrows-outgoings ;; cursor for fetching outgoings set
  arrows-incomings
  )


(set-record-type-printer! <culturia>
                          (lambda (record port)
                            (format port
                                    "<culturia ~s>"
                                    (culturia-connection record))))

;; ---


(define (culturia-init connection)
  (let ((session (session-open connection)))
    ;; create table to store revision informations
    (session-create session
                    "table:revisions"
                    (string-append "key_format=r,"
                                   "value_format=QSS,"
                                   "columns=(uid,parent,name,comment)"))

    ;; this index is useful to retrieve revisions by name
    (session-create session "index:revisions:names" "columns=(name)")

    ;; create table to store culture informations
    (session-create session
                    "table:cultures"
                    (string-append "key_format=r,"
                                   "value_format=QQSS,"
                                   "columns=(uid,revision,parent,name,comment)"))
    ;; this index is useful to retrieve cultures by name
    ;; XXX: not sure it's useful since at runtime the cultures of a given
    ;; revision are cached
    (session-create session "index:cultures:names" "columns=(name)")

    ;; create a main table to store atom informations
    (session-create session
                    "table:atoms"
                    (string-append "key_format=r,"
                                   "value_format=QQQSSS,"
                                   "columns="
                                   (string-append "("
                                                  "uid,"
                                                  "revision,"
                                                  "deleted,"
                                                  "culture,"
                                                  "type,"
                                                  "name,"
                                                  "data,"
                                                  ")")))

    ;; create related indices

    ;; this index is useful to retrieve existing atoms in a given workspace and a given culture
    (session-create session "index:atoms:revisions" "columns=(revision)")
    (session-create session "index:atoms:cultures" "columns=(culture)")

    ;; this index is useful to retrieve atom of given type name
    (session-create session "index:atoms:types" "columns=(type)")
    ;; this index is useful to retrieve an <atom> of given type and name
    (session-create session "index:atoms:type-names" "columns=(type,name)")

    ;; create a main table to store <arrow>
    (session-create session
                    "table:arrows"
                    (string-append "key_format=r,"
                                   "value_format=QQQQ,"
                                   "columns=(uid,revision,deleted,start,end)"))
    ;; this index is useful to traverse outgoing set
    (session-create session "index:arrows:outgoings" "columns=(start)")
    ;; this index is useful to traverse incoming set
    (session-create session "index:arrows:incomings" "columns=(end)")

    (let ((revisions (cursor-open session "table:revisions"))
          (revisions-tree (create-tree 0)))
      ;; build revisions tree
      (cursor-reset revisions)
      (let loop ()
        (if (not (cursor-next revisions))
            revisions-tree
            (match (append (cursor-key-ref revisions) (cursor-value-ref revisions))
              ((uid revision parent _ _)
               (tree-append! revisions-tree parent uid)
               (loop))
              (_ (Oops!)))))

      (make-culturia connection
                     session
                     ;; <revision>
                     revisions
                     (cursor-open session "table:revisions" "append")
                     (cursor-open session "index:revisions:names(uid,parent,comment)")
                     revisions-tree
                     ;; <culture>
                     (cursor-open session "table:cultures")
                     (cursor-open session "table:cultures" "append")
                     (cursor-open session "index:cultures:names(uid,revision,parent,comment)")
                     ;; <atom> cursors
                     (cursor-open session "table:atoms")
                     (cursor-open session "table:atoms" "append")
                     (cursor-open session "index:atoms:revisions(uid,deleted)")
                     (cursor-open session "index:atoms:cultures(uid,revision,deleted)")
                     (cursor-open session "index:atoms:types(uid,revision,culture,deleted)")
                     (cursor-open session "index:atoms:type-names(uid,revision,culture,deleted)")
                     ;; <arrow> cursor
                     (cursor-open session "table:arrows")
                     (cursor-open session "table:arrows" "append")
                     (cursor-open session "index:arrows:outgoings(uid,revision,deleted)")
                     (cursor-open session "index:arrows:incomings(uid,revision,deleted)")))))


(define-public (create-culturia path)
  "Create and initialize a culturia database at PATH and return a <culturia>"

  (define (now)
    (date->string (current-date) "~Y-~m-~d ~H:~M:~s"))

  (define (path-exists? path)
    "Return #true if path is a file or directory. #false if it doesn't exists"
    (access? path F_OK))

  (when (path-exists? path)
    (raise "There is already something at ~a. Use (open-culturia path) instead" path))

  (mkdir path)
  (open-culturia path))


(define-public (open-culturia path)
  "Initialize a culturia database at PATH; creating if required the tables and
   indices. Return a <culturia> record."
  (let ((connection (connection-open path "create")))
    (culturia-init connection)))


(define-public (culturia-close culturia)
  (connection-close (culturia-connection culturia)))


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

;;; <revision>


(define-record-type* <revision> culturia uid parent name comment path cultures-tree)


(define (make-cultures-tree culturia revisions)
  (let ((cursor (culturia-cultures culturia))
        (cultures-tree (create-tree 0)))
    (cursor-reset cursor)
    (let loop ()
      (when (cursor-next cursor)
        (match (append (cursor-key-ref cursor) (cursor-value-ref cursor))
          ((uid revision parent name comment)
           ;; XXX: this does not handle
           ;; - deleted culture
           ;; - updated culture
           (when (list-index revisions revision)
             (tree-append! cultures-tree parent uid))
           (loop))
          (_ (Oops!)))))
    cultures-tree))


(define-public (checkout-revision culturia name)
  (let ((cursor (culturia-revisions-names culturia)))
    ;; look for revision by name
    (cursor-key-set cursor name)
    (if (cursor-search cursor)
        ;; retrieve revision information
        (match (append (cursor-key-ref cursor) (cursor-value-ref cursor))
          ((name uid parent name comment)
           (let* ((path (tree-path (culturia-revision-tree culturia) uid))
                  (cultures-tree (make-cultures-tree culturia path)))
             (make-revision culturia uid parent name comment path cultures-tree)))
          (_ (Oops!)))
        ;; otherwise create a new revision
        ;; FIXME: done unconditionally
        (let ((cursor (culturia-revisions-append culturia)))
          (cursor-value-set cursor 0 name "*branch*")
          (cursor-insert cursor)
          (let ((uid (cursor-key-ref cursor))
                (revisions-tree (culturia-revisions-tree culturia)))
            (tree-append! revisions-tree 0 uid)
            (make-revision culturia
                           uid
                           0
                           name
                           "*branch*"
                           (list uid)
                           (create-tree 0)))))))
  
;; FIXME: debug
(define-public (revision-commit revision comment)
  (let* ((culturia (revision-culturia revision))
         (revisions (culturia-revisions-append culturia))
         (cultures (culturia-cultures culturia))
         (parent (revision-uid revision))
         (cultures-tree (create-tree 0)))
    ;; add revision to the storage
    (cursor-value-set revisions parent name comment)
    (cursor-insert revisions)
    (let ((culturia (revision-culturia revision))
          (uid (cursor-key-ref cursor))
          (path (cons uid (revision-path revision)))
          ;; create a <revision>
          ;; FIXME: replace with a <tree> procedure (tree-copy tree)
          (cultures-tree (make-cultures-tree culturia path)))
      (make-revision culturia uid parent name comment path cultures-tree))))


;; ---


(define-record-type* <culture> revision uid parent name comment)


(define (create-culture revision name)
  ;; FIXME: add culture to revision cache
  (let* ((culturia (revision-culturia revision))
         (cursor (culturia-cultures-append culturia))
         (cultures-tree (revision-cultures-tree revision)))
    ;; add the culture to the cultures tables
    (cursor-value-ref (1+ (revision-uid revision)) 0 name "*culture*")
    (cursor-insert cursor)
    ;; update cultures-tree
    (let ((uid (cursor-key-ref cursor)))
      (tree-append! cultures-tree 0 uid)
      (make-culture revision uid 0 name "*culture*"))))


(define (checkout-culture revision name)
  ;; FIXME: this doesn't take into account the revision...
  (let ((cursor (culturia-cultures-names culturia)))
    (cursor-key-set cursor name)
    (when (cursor-search cursor)
      (match (cursor-value-ref cursor)
        [(uid _ parent comment)
         (make-culture revision uid parent name comment)]))))


(define (culture-atoms culture))


  
;; ;; ---

;; (define-record-type <atom>
;;   (make-atom culture uid type name data outgoings incomings)
;;   (culture atom-culture)
;;   (uid atom-uid)
;;   (type atom-type)
;;   (name atom-name)
;;   (data atom-data-ref atom-data-set)
;;   (outgoings atom-outgoings-ref atom-outgoings-set)
;;   (incomings atom-incomings-set atom-incomings-set))


;; (define (create-atom culture type name data)
;;   (let* ((workspace (culture-workspace culture))
;;          (cursor (culturia-atoms-append (workspace-culturia workspace))))
;;     (cursor-set-value (culture-path culture)
;;                       (workspace-revision workspace)
;;                       (workspace-path-suffix workspace)
;;                       type
;;                       name
;;                       (scm->string data)
;;                       #false)
;;     (cursor-insert cursor)
;;     (make-atom culture (cursor-key-ref cursor) type name data '() '())))


;; (define (atom-delete culture atom)
;;   ;; mark the atom as deleted for the current workspace (revision+branch)
;;   (let* ((workspace (culture-workspace culture))
;;          (cursor (culturia-atoms (workspace-culturia workspace))))
;;     (cursor-set-key cursor (atom-uid atom))
;;     (cursor-set-value (culture-path culture)
;;                       (workspace-revision workspace)
;;                       (workspace-path-suffix workspace)
;;                       (atom-type type)
;;                       (atom-name name)
;;                       (scm->string (atom-data atom))
;;                       #true)
;;     (cursor-update cursor)))


;; ;; ---

;; (define-record-type* <culture> culturia uid name)
