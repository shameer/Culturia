(define-module (culturia))

(use-modules (culturia wiredtiger))

(use-modules (srfi srfi-1))
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
  (gensym (string-append "culturia-" name "-")))

(define *exception* (make-exception "exception"))

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
  ;; <atom> cursors
  atoms  ;; main cursor all the atoms used for direct access via uid
  atoms-append  ;; secondary cursor for insert
  atoms-types
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
    ;; create a main table to store atom informations
    (session-create session
                    "table:atoms"
                    (string-append "key_format=r,"
                                   "value_format=SSS,"
                                   "columns="
                                   (string-append "("
                                                  "uid,"
                                                  "type,"
                                                  "name,"
                                                  "assoc,"
                                                  ")")))

    ;; create related indices

    ;; this index is useful to retrieve atom of given type name
    (session-create session "index:atoms:types" "columns=(type)")
    ;; this index is useful to retrieve an <atom> of given type and name
    (session-create session "index:atoms:type-names" "columns=(type,name)")

    ;; create a main table to store <arrow>
    (session-create session
                    "table:arrows"
                    (string-append "key_format=r,"
                                   "value_format=QQ,"
                                   "columns=(uid,start,end)"))
    ;; this index is useful to traverse outgoing set
    (session-create session "index:arrows:outgoings" "columns=(start)")
    ;; this index is useful to traverse incoming set
    (session-create session "index:arrows:incomings" "columns=(end)")

    (make-culturia connection
                   session
                   ;; <atom> cursors
                   (cursor-open session "table:atoms")
                   (cursor-open session "table:atoms" "append")
                   (cursor-open session "index:atoms:types(uid)")
                   (cursor-open session "index:atoms:type-names(uid)")
                   ;; <arrow> cursor
                   (cursor-open session "table:arrows")
                   (cursor-open session "table:arrows" "append")
                   (cursor-open session "index:arrows:outgoings(uid,end)")
                   (cursor-open session "index:arrows:incomings(uid,start)"))))


(define-public (culturia-open path)
  "Initialize a culturia database at PATH; creating if required the tables and
   indices. Return a <culturia> record."
  (let ((connection (connection-open path "create")))
    (culturia-init connection)))


(define-public (culturia-create path)
  "Create and initialize a culturia database at PATH and return a <culturia>"

  (define (path-exists? path)
    "Return #true if path is a file or directory. #false if it doesn't exists"
    (access? path F_OK))

  (when (path-exists? path)
    (raise "There is already something at ~a. Use (culturia-open path) instead" path))

  (mkdir path)
  (culturia-open path))


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

;;; <atoms>


(define-record-type <atom>
  (make-atom culturia uid type name assoc)
  atom?
  (culturia atom-culturia)
  (uid atom-uid)
  (type atom-type)
  (name atom-name)
  (assoc atom-assoc %atom-assoc-set!))


(export atom-uid atom-type atom-name atom-assoc)


(define-public (culturia-atom-create culturia type name)
  (let ((cursor (culturia-atoms-append culturia)))
    (cursor-value-set cursor type name (scm->string (list)))
    (cursor-insert cursor)
    (make-atom culturia (car (cursor-key-ref cursor)) type name (list))))


(define-public (culturia-atom-ref/uid culturia uid)
  (let ((cursor (culturia-atoms culturia)))
    (cursor-key-set cursor uid)
    (when (not (cursor-search cursor))
      (Oops!))
    (match (cursor-value-ref cursor)
      [(type name assoc) (make-atom culturia uid type name (string->scm assoc))])))


(define (culturia-atom-ref/type+name culturia type name)
  (let ((cursor (culturia-atoms-type-names culturia))
        (match? (lambda (other)
                  (equal? other (list type name)))))
    (cursor-key-set cursor type name)
    (if (cursor-search cursor)
        (culturia-atom-ref/uid culturia (car (cursor-value-ref cursor)))
        #nil)))


(define-public (culturia-atom-ref/type culturia type)
  (let ((cursor (culturia-atoms-types culturia)))
    (cursor-key-set cursor type)
    (if (cursor-search cursor)
        (let loop ((atoms (list)))
          (if (equal? (cursor-key-ref cursor) (list type))
              (let ((atoms (append (cursor-value-ref cursor) atoms)))
                (if (cursor-next cursor)
                    (loop atoms)
                    atoms))
              atoms))
        (list))))


(define*-public (culturia-atom-ref culturia type #:optional name)
  (if name
      (culturia-atom-ref/type+name culturia type name)
      (culturia-atom-ref/type culturia type)))


(define-public (atom-assoc-set! atom key value)
  (let* ((assoc (atom-assoc atom))
         (assoc (alist-delete key assoc))
         (assoc (acons key value assoc))
         (cursor (culturia-atoms (atom-culturia atom))))
    (cursor-key-set cursor (atom-uid atom))
    (when (not (cursor-search cursor))
      (Oops!))
    (cursor-value-set cursor (atom-type atom) (atom-name atom) (scm->string assoc))
    (cursor-update cursor)
    (%atom-assoc-set! atom assoc)))


(define-public (atom-assoc-ref atom key)
  (assoc-ref (atom-assoc atom) key))


(define-public (atom-link atom other)
  (let ((cursor (culturia-arrows-append (atom-culturia atom))))
    (cursor-value-set cursor (atom-uid atom) (atom-uid other))
    (cursor-insert cursor)))
         

(define (atom-arrow atom cursor)
  (let ((uid (atom-uid atom)))
    (cursor-key-set cursor uid)
    (if (cursor-search cursor)
        (let loop ((atoms (list)))
          (if (eq? (car (cursor-key-ref cursor)) uid)
              (match (cursor-value-ref cursor)
                ((_ uid)
                 (let ((atoms (cons uid atoms)))
                   (if (cursor-next cursor)
                       (loop atoms)
                       atoms))))
              atoms))
        (list))))


(define-public (atom-outgoings atom)
  (atom-arrow atom (culturia-arrows-outgoings (atom-culturia atom))))


(define-public (atom-incomings atom)
  (atom-arrow atom (culturia-arrows-incomings (atom-culturia atom))))

;;
;; FIXME: test this and uncomment 
;;
;; (define-public (atom-delete atom)
;;   (let* ((culturia (atom-culturia atom))
;;          (atoms (culturia-atoms culturia))
;;          (arrows (culturia-arrows culturia))
;;          (outgoings (culturia-arrows-outgoings culturia))
;;          (incomings (culturia-arrows-incomings culturia)))
;;     ;; remove atom entry
;;     (cursor-key-set atoms (atom-uid atom))
;;     (cursor-remove atoms)
;;     ;; remove outgoings arrows
;;     (let ((remove
;;            (lambda (cursor)
;;              (cursor-key-set cursor)
;;              (when (cursor-search cursor)
;;                (let loop ()
;;                  (if (eq? (car (cursor-key-ref cursor)) uid)
;;                      (match (cursor-value-ref cursor)
;;                        ((uid _)
;;                         (cursor-key-set atoms uid)
;;                         (cursor-search atoms)
;;                         (cursor-remove atmos)
;;                         (loop)))))))))
;;       (remove outgoings)
;;       (remove incomings))))
