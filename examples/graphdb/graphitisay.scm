;; graphitisay.

;; Copyright © 2014-2015 Amirouche BOUBEKKI <amirouche@hypermove.net>

;; graphitisay is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.

;; graphitisay is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with graphitisay.  If not, see <http://www.gnu.org/licenses/>
(define-module (graphitisay))

(use-modules (srfi srfi-1))  ;; lists
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-field

(use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 match))  ;; match

(use-modules (wiredtiger))


;;;
;;; Guile path helpers
;;;

;; XXX: maybe not be much useful in fact

(define-public (path-exists? path)
  "Return #true if path is a file or directory.
   #false if it doesn't exists"
  (access? path F_OK))

(define-public (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be  prepend"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))

(define-public (path-split path)
  (let ((parts (string-split path #\/)))
    (if (equal? (car parts) "")
        (cons (string-append "/" (cadr parts)) (cddr parts))
        parts)))

(define*-public (path-mkdir dirpath #:optional (parents #false))
  "Create DIRPATH directory its parents if PARENTS is true"
  (if parents
      (let* ((parts (path-split dirpath))
             (paths (let loop ((dirs (cdr parts))
                               (out (list (car parts))))
                      (if (null? dirs)
                          (reverse out)
                          (loop (cdr dirs) (cons (apply path-join (list (car out) (car dirs))) out))))))
        (and (map (lambda (p) (if (not (path-exists? p)) (mkdir p))) paths) #true))
      (if (not (path-exists? dirpath)) (and (mkdir dirpath) #true))))



;;;
;;; Guile helpers
;;;
;;
;; macro to quickly define immutable records
;;
;; FIXME: Taken from Guile (maybe should be in (srfi srfi-99))
;;        adapted to make it possible to declare record type like `<abc>' and keep
;;        field accessor bracket free. record name *must* have brackets or everything
;;        is broken
;;
;; Usage:
;;
;;   (define-record-type <abc> field-one field-two)
;;   (define zzz (make-abc 1 2))
;;   (abc-field-one zzz) ;; => 1
;;
;; FIXME: maybe this is less useful than the immutable record of (srfi srfi-9 gnu)
;;        I still use `set-field` and `set-fields`
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

;;;
;;; Graph helpers
;;;

;; association helpers

(define (assoc-del assoc key)
  "Return a copy of ASSOC where the KEY association doesn't exists"
  (alist-delete key assoc))

(define (assoc-set assoc key value)
  "Return a copy of ASSOC where KEY is set to VALUE"
  (alist-cons key value (assoc-del assoc key)))

(define (assoc-ref assoc key)
  "Return the value of KEY in ASSOC, if KEY is not found return #nil"
  (let loop ((assoc assoc))
    (if (null? assoc)
        #nil
        (if (equal? (caar assoc) key)
            (cdar assoc)
            (loop (cdr assoc))))))

;;;
;;; Properties
;;;
;;
;; assoc that keeps track of which key were added, modified *and* removed
;;

(define (assoc->properties assoc)
  (map (lambda (pair) (match pair [(key . value) (list key (list value))])) assoc))

(define (properties->assoc properties)
  (fold (lambda (triplet assoc)
          (match triplet
            [(key (old new)) (if new (acons key new assoc) assoc)]
            [(key (old)) (acons key old assoc)]))
        '()
        properties))

;; (define assoc (assoc-set (assoc-set '() 'key 'value) 'some 'thing))
;; (define props (assoc->properties assoc))
;; (properties->assoc props)

(define (properties-ref-select properties key select)
  (let loop ((properties properties))
    (if (null? properties)
        #nil
        (let ((triplet (car properties)))
          (match triplet
            [(other (old new)) (if (equal? key other)
                                   (select #true new old)
                                   (loop (cdr properties)))]
            [(other (old)) (if (equal? key other)
                               (select #false #nil old)
                               (loop (cdr properties)))])))))

(define (properties-new properties key)
  (properties-ref-select properties key (lambda (has-new new old) new)))

(define (properties-old properties key)
  (properties-ref-select properties key (lambda (has-new new old) old)))

(define (properties-ref properties key)
  (properties-ref-select properties key (lambda (has-new new old) (if has-new new old))))

(define (properties-set properties key value)
  (map (lambda (triplet)
         (match triplet
           [(other (old new)) (if (equal? key other)
                                  (list other (list old value))
                                  triplet)]
           [(other (old)) (if (equal? key other)
                              (list other (list old value))
                              triplet)]))
       properties))

(define (properties-del properties key)
  (properties-set properties key #nil))

;; (define props (properties-set props 'key 'bar))
;; (properties-new props 'key)
;; (properties-old props 'key)
;; (properties-new props 'some)
;; (properties-ref props 'key)
;; (properties-ref props 'some)
;; (properties->assoc props)


;;;
;;; key/value records helpers
;;;

(define (read-value value)
  "serialize VALUE with `read` as scheme objects"
  (with-input-from-string (car (unpack "S" value)) (lambda () (read))))

(define (write-value value)
  "Write VALUE in a string and return it"
  (pack "S" (with-output-to-string (lambda () (write value)))))

;;;
;;; collection
;;;
;;
;; Collection have both key and value format set to `S`. Operations
;; are done against their cursor found in the related txn.
;;

(define (collection-ref cursor key)
  "Return the scheme value associated with KEY in CURSOR"
  (cursor-key-set cursor key)
  (if (cursor-search cursor)
      (let ((value (cursor-value-ref cursor)))
        (cursor-reset cursor)
        (read-value value))
      #nil))

(define (collection-set! cursor key value)
  "Set KEY to VALUE in the collection associated with CURSOR"
  (cursor-key-set cursor key)
  (cursor-value-set cursor (write-value value))
  (cursor-update cursor)
  cursor-reset cursor)

(define (collection-insert! cursor key value)
  "Set KEY to VALUE in the collection associated with CURSOR"
  (cursor-key-set cursor key)
  (cursor-value-set cursor (write-value value))
  (cursor-insert cursor)
  (cursor-reset cursor))

(define (collection-del! cursor key)
  (cursor-key-set cursor key)
  (if (cursor-search cursor)
      (begin
        (cursor-remove cursor)
        (cursor-reset cursor))))

(define (collection-all cursor)
  (cursor-key-set cursor "")
  (if (cursor-reset cursor)
      (let loop ((collection '()))
        (if (cursor-next cursor)
            (loop (acons (car (cursor-get-key cursor))
                         (read-value (cursor-value-ref cursor))
                         collection))
            collection))))

;;;
;;; labels
;;;
;;
;; A collection where keys are two string column and values are empty
;; Keys are built from the label and the identifier.
;;

(define (labels-all cursor label)
  "Return the scheme value associated with KEY in CURSOR"
  (cursor-key-set cursor thelabel "")
  (if (cursor-search-near cursor)
      (let ((out (let loop ((out '()))
                   (if (cursor-next)
                       (match (cursor-key-ref cursor)
                         [(other uid) (if (equal? other label)
                                          (loop (cons uid out)))])
                       out))))
        (cursor-reset)
        out)
      (begin (cursor-reset cursor)
             '())))

(define (labels-insert! cursor label identifier)
  "Set KEY to VALUE in the label set associated with CURSOR"
  (cursor-key-set cursor label identifier)
  (cursor-value-set cursor (write-value ""))
  (cursor-insert cursor)
  (cursor-reset cursor))

(define (labels-del! cursor label identifier)
  (cursor-key-set cursor label identifier)
  (if (cursor-search cursor)
      (begin
        (cursor-remove cursor)
        (cursor-reset cursor))))

;;;
;;; procedure to generate unique identifiers aka. uid
;;;

;; init random with a random state

(set! *random-state* (random-state-from-platform))

(define (generate-uid exists?)
  "Generate a random string made up alphanumeric ascii chars that doesn't exists
   according to `exists?`"
  (define CHARS "0123456789AZERTYUIOPQSDFGHJKLMWXCVBN")

  (define (random-id)
    ;; append 8 alphanumeric chars from `CHARS`
    (let loop ((count 8)
               (id ""))
      (if (eq? count 0)
          id
          (loop (1- count) (format #f "~a~a" id (string-ref CHARS (random 36)))))))

  (let loop ()
    ;; generate a random uid until it find an id that doesn't already exists?
    (let ((id (random-id)))
      (if (exists? id) (loop) id))))

;;;
;;; Graph
;;;

(define-record-type* <graph> dir connection indices)

(define*-public (create-graph dir #:optional (indices '()))
  "Create a graph inside the directory DIR"
  (path-mkdir dir #true)
  (make-graph dir (connection-open dir "create") indices))

(define-public (graph-close graph)
  "Close a graph database GRAPH"
  (connection-close (graph-connection graph)))

;;;
;;; Node
;;;

(define-record-type* <node> txn uid incoming-uids label outgoing-uids properties-properties)

(export node-uid node-incoming-uids node-label node-outgoing-uids)

(set-record-type-printer! <node>
                          (lambda (record port)
                            (format port
                                    "<<node> uid: ~a label: ~a>"
                                    (node-uid record)
                                    (node-label record))))

(define-public (node-outgoings node)
  "Return the list of edge leaving NODE"
  (map (lambda (uid) (txn-edge-ref (node-txn node)) uid) (node-outgoing-uids node)))

(define-public (node-incomings node)
  "Return the list of edges coming to NODE"
  (map (lambda (uid) (txn-edge-ref (node-txn txn) uid)) (node-incoming-uids node)))

(define-public (node->value node)
  `((uid ,uid)
    (incomings ,(node-incoming-uids node))
    (label ,(node-label node))
    (outgoings ,(node-outgoing-uids node))
    (properties ,(node-properties node))))

(define-public (value->node txn uid value)
  (make-node txn
             uid
             (assoc-ref value 'incomings)
             (assoc-ref value 'label)
             (assoc-ref value 'outgoings)
             (assoc->properties (assoc-ref value 'properties))))

;; properties

(define-public (node-properties node)
  (properties->assoc (node-properties-properties node)))

(define-public (node-properties-set node key value)
  (let ((properties (properties-set (node-properties-properties node) key value)))
    (set-field node (node-properties-properties) properties)))

(define-public (node-properties-ref node key)
  (properties-ref (node-properties-properties node) key))

(define-public (node-properties-del node key)
  (let ((properties (node-properties-del (node-properties-properties node) key)))
    (set-field node (node-properties-properties) value)))

;;;
;;; Edge
;;;

(define-record-type* <edge> txn uid start-uid label end-uid properties-properties)

(export edge-uid edge-start-uid edge-label edge-end-uid edge-properties)

(set-record-type-printer! <edge>
                          (lambda (record port)
                            (format port
                                    "<<edge> uid: ~a start: ~a label: ~a end: ~a>"
                                    (edge-uid record)
                                    (edge-start-uid record)
                                    (edge-label record)
                                    (edge-end-uid record))))

(define-public (edge-start edge)
  (txn-vertex-ref (edge-txn edge) (edge-start-uid edge)))

(define-public (edge-end edge)
  (txn-vertex-ref (edge-txn edge) (edge-end-uid edge)))

(define-public (edge->value edge)
  `((uid ,uid)
    (start ,(edge-start-uid edge))
    (label ,(edge-label edge))
    (end ,(-end-uid edge))
    (properties ,(edge-properties edge))))

;; properties

(define-public (edge-properties edge)
  (properties->assoc (edge-properties-properties edge)))

(define-public (edge-properties-set edge key value)
  (let ((properties (properties-set (edge-properties-properties edge) key value)))
    (set-field edge (edge-properties-properties) properties)))

(define-public (edge-properties-ref edge key)
  (properties-ref (edge-properties-properties edge) key))

(define-public (edge-properties-del edge key)
  (let ((properties (edge-properties-del (edge-properties-properties edge) key)))
    (set-field edge (edge-properties-properties) value)))

;;
;; transaction
;;

(define-record-type* <txn> session indices nodes node-labels edges edge-labels properties)

(define*-public (txn-begin graph #:optional (isolation ""))
  "Start a transaction with GRAPH.

   The transaction is not supposed to be shared between threads"
  (define cnx (graph-connection graph))
  (define session (session-open cnx isolation))

  (session-create session "table:nodes" "key_format=S,value_format=S")
  (session-create session "table:node-labels-index" "key_format=SS,value_format=S")
  (session-create session "table:edges" "key_format=S,value_format=S")
  (session-create session "table:edge-labels-index" "key_format=SS,value_format=S")
  (session-create session "table:properties-index" "key_format=S,value_format=S")

  (let ((txn (make-txn session
                       (graph-indices graph)
                       ;; XXX: Cursors must be cached for better performance.
                       ;; XXX: theorically they can be several cursors
                       ;; per session per table. This is useful to be lazy
                       ;; e.g. during a join. In the context of graphitisay
                       ;; queries over those tables dubbed `collection`
                       ;; retrieve results. They are not lazy.
                       ;; cursors are stateful, so they can lead to bizar behavior
                       ;; luckly this is not happening in graphitisay because of the
                       ;; above
                       (cursor-open session "table:nodes" "raw")
                       (cursor-open session "table:node-labels-index" "raw")
                       (cursor-open session "table:edges" "raw")
                       (cursor-open session "table:edge-labels-index" "raw")
                       (cursor-open session "table:properties-index" "raw"))))
    (session-transaction-begin session)
    txn))

(define-public (txn-commit txn)
  (session-transaction-commit (txn-session txn))
  (session-close (txn-session txn)))

(define-public (txn-abort txn)
  (session-transaction-abort (txn-session txn))
  (session-close (txn-session txn)))

;;; txn node

(define*-public (txn-create-node txn label #:optional (properties '()))
  "Create a new NODE using TXN with LABEL and initial PROPERTIES.
   The object is not persisted but has valid unique identifier."
  (let* ((exists? (lambda (uid) (not (null? (collection-ref (txn-nodes txn) uid)))))
         (uid (generate-uid exists?))
         (node (make-node txn uid '() label '() (assoc->properties properties))))
    ;; save node
    (collection-set! (txn-nodes txn)
                     (node-uid node)
                     (node->value node))
    ;; index node by its label
    (labels-insert! (txn-node-labels-index txn)
                    label
                    uid)
    node))

(define-public (txn-node-ref txn uid)
  "Return the node with UID as unique identifier"
  (value->node txn uid (collection-ref (txn-nodes txn) uid)))

;; (define-public (txn-nodes-label txn label)
;;   """Return all nodes with the given LABEL"""
;;   (map (lambda (uid) (txn-node-ref txn uid)) (labels-all (txn-node-labels txn) label)))

;; (define-public (txn-nodes-all txn)
;;   """Return all nodes"""
;;   (map (lambda (entry) (make-node txn (car entry) (cdr entry))) (collection-all (txn-nodes txn))))

;; (define-public (node-save node)
;;   "Update NODE using TXN. NODE must have a unique identifier (uid).
;;    NODE must have been retrieved with `txn-create-node` or `txn-node-ref`."
;;   (collection-set! (txn-nodes (node-txn node)) (node-uid node) (node-value node))
;;   node)

;;; txn edge

(define*-public (txn-create-edge! txn start label end #:optional (properties '()))
  "Create a new EDGE using TXN with LABEL and initial PROPERTIES. START and END must be
   nodes returned with `txn-create-node` or `txn-node-ref` the same transaction"
  (let* ((exists? (lambda (uid) (not (null? (collection-ref (txn-edges txn) uid)))))
         (uid (generate-uid exists?))
         (edge (make-edge txn
                          uid
                          (node-start-uid start)
                          label
                          (node-end-uid end)
                          (assoc->properties properties)))

         ;;; add edge to `outgoings` of `start` node
         (outgoings (cons (node-uid start) (node-outgoing-uids start)))
         (start (set-field outgoings (node-outgoing-uids) start))
         ;; save change
         (_ (collection-set! (txn-nodes txn) (node-uid start) (node->value start)))
         
         ;;; add edge to `incomings` of `end` node
         (incomings (cons (node-uid end) (node-incoming-uids end)))
         (end (set-field incomings (node-incoming-uids) end))
         ;; save change
         (_ (collection-set! (txn-nodes txn) (node-uid end) (node->value end))))
    ;; save edge
    (collection-insert! (txn-edges txn) uid (edge->value edge))
    ;; index edge by label
    (labels-insert! (txn-edge-labels-index txn) label uid)
    edge))

(define-public (txn-edge-ref txn uid)
  "Return the edge with UID as unique identifier"
  (edge-> txn uid (collection-ref (txn-edges txn) uid)))

;; (define-public (txn-edges-label txn label)
;;   """Return all nodes with the given LABEL"""
;;   (filter (lambda (edge) ((equal? 

;; (define-public (txn-edge-all txn)
;;   (map (lambda (entry) (make-edge txn (car entry) (cdr entry))) (collection-all (txn-edges txn))))

;; (define-public (edge-save edge)
;;   "Update EDGE using TXN. EDGE must have a unique identifier (uid).
;;    EDGE must have been retrieved with `txn-create-edge` or `txn-edge-ref`."
;;   (collection-set! (txn-edges (edge-txn edge)) (edge-uid edge) (edge-value edge))
;;   edge)
