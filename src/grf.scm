;; grf.scm

;; Copyright © 2014-2016 Amirouche BOUBEKKI <amirouche@hypermove.net>

;; grf is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.

;; grf is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with grf.  If not, see <http://www.gnu.org/licenses/>
(define-module (grf))

(use-modules (srfi srfi-1))  ;; lists
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-field
(use-modules (srfi srfi-41))  ;; streams

;; (use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 match))
(use-modules (ice-9 receive))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))

;;;
;;; plain records
;;;
;;
;; macro to quickly define immutable records
;;
;;
;; Usage:
;;
;;   (define-record-type <car> seats wheels)
;;   (define smart (make-car 2 4))
;;   (car-seats smart) ;; => 2
;;
;; Mutation is *not* done in place, via set-field or set-fields eg.:
;;
;; (define smart-for-4 (set-field smart (seats) 4))
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
;;; Grf
;;;

(define-public *grf* '((vertex
                        ((uid . record))
                        ((label . string)
                         (assoc . string))
                        ((label (label) (uid))))
                       (vertex-index
                        ((uid . unsigned-integer)
                         (key . string))
                        ((value . string))
                        ((avu (key value) (uid))))
                       (edge
                        ((uid . record))
                        ((start . unsigned-integer)
                         (label . string)
                         (end . unsigned-integer)
                         (assoc . string))
                        ((label (label) (uid))
                         (outgoings (start) (uid))
                         (incomings (end) (uid))))
                       (edge-index
                        ((uid . unsigned-integer)
                         (key . string))
                        ((value . string))
                        ((avu (key value) (uid))))))

;;; vertex-index and edge-index helpers

(define (index-insert! cursor uid assoc)
  (let loop ((assoc assoc))
    (unless (null? assoc)
      (cursor-insert* cursor (list uid (symbol->string (caar assoc))) (list (scm->string (cdar assoc))))
      (loop (cdr assoc)))))

(define (assoc-diff old new)
  (values (lset-difference eq? (map car new) (map car old)) ; added
          (lset-difference equal? new (lset-intersection equal? new old)) ; updated
          (lset-difference eq? (map car old) (map car new)))) ; deleted

(define (index-update! cursor uid old new)
  (receive (added updated deleted) (assoc-diff old new)
    (index-insert! cursor uid added)
    (let update ((updated updated))
      (unless (null? updated)
        (cursor-update* cursor
                        (list uid (symbol->string (caar updated)))
                        (list (scm->string (cdar updated))))
        (update (cdr updated))))
    (let delete ((deleted deleted))
      (unless (null? deleted)
        (cursor-remove* cursor uid (symbol->string (car deleted)))
        (delete (cdr deleted))))))

;;;
;;; Vertex
;;;

(define-record-type* <vertex> uid label assoc initial-assoc)

(export vertex-uid vertex-assoc)

(set-record-type-printer! <vertex>
                          (lambda (record port)
                            (format port
                                    "<vertex uid: ~a label: ~a>"
                                    (vertex-uid record)
                                    (vertex-label record))))

(define-public (vertex-ref context uid)
  (let ((cursor (context-ref context 'vertex)))
    (match (cursor-value-ref* cursor uid)
      (() (throw 'vertex-not-found uid))
      ((label assoc)
       (let ((assoc (string->scm assoc)))
         (make-vertex uid label assoc assoc))))))

(define-public (vertex-label? edge label)
  (equal? (vertex-label edge) label))

(define-public (vertex-add! context label assoc)
  (let ((uid (call-with-cursor context 'vertex-append
               (lambda (cursor)
                 (cursor-insert* cursor '() (list label (scm->string assoc)))))))
    (call-with-cursor context 'vertex-index
      (lambda (cursor)
        (index-insert! cursor uid assoc)))
    (make-vertex uid label assoc assoc)))

(define-public (vertex-assoc-set vertex key value)
  (let* ((assoc (vertex-assoc vertex))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field vertex (vertex-assoc) assoc)))

(define-public (vertex-assoc-ref vertex key)
  (assoc-ref (vertex-assoc vertex) key))

(define-public (vertex-save! context vertex)
  (call-with-cursor context 'vertex
    (lambda (cursor)
      (cursor-update* cursor
                      (list (vertex-uid vertex))
                      (list (vertex-label vertex) (scm->string (vertex-assoc vertex))))))
  (call-with-cursor context 'vertex-index
    (lambda (cursor)
      (index-update! cursor (vertex-uid vertex) (vertex-initial-assoc vertex) (vertex-assoc vertex)))))

(define-public (vertex-outgoings context uid)
  (let ((cursor (context-ref context 'edge-outgoings)))
    (map car (cursor-range cursor uid))))

(define-public (vertex-incomings context uid)
  (let ((cursor (context-ref context 'edge-incomings)))
    (map car (cursor-range cursor uid))))

(define-public (vertex-with-label context label)
  (let ((cursor (context-ref context 'vertex-label)))
    (map cadr (cursor-range cursor label))))

;;;
;;; Edge
;;;

(define-record-type* <edge> uid start label end assoc initial-assoc)

(export edge-uid edge-start edge-label edge-end edge-assoc)

(set-record-type-printer! <edge>
                          (lambda (record port)
                            (format port
                                    "<edge uid: ~a start: ~a label: ~a end: ~a>"
                                    (edge-uid record)
                                    (edge-start record)
                                    (edge-label record)
                                    (edge-end record))))

(define-public (edge-ref context uid)
  (let ((cursor (context-ref context 'edge)))
    (match (cursor-value-ref* cursor uid)
      (() (throw 'edge-not-found uid))
      ((start label end assoc)
       (let ((assco (string->scm assoc)))
         (make-edge uid start label end assoc assoc))))))

(define-public (edge-label? edge label)
  (equal? (edge-label edge) label))

(define-public (edge-add! context start label end assoc)
  (let ((uid (call-with-cursor context 'edge-append
               (lambda (cursor)
                 (cursor-insert* cursor #nil (list start label end (scm->string assoc)))))))
    (call-with-cursor context 'vertex-index
      (lambda (cursor)
        (index-insert! cursor uid assoc)))
    (make-edge uid start label end assoc assoc)))

(define-public (edge-assoc-update edge key value)
  (let* ((assoc (edge-assoc edge))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field edge (edge-assoc) assoc)))

(define-public (edge-assoc-ref edge key)
  (assoc-ref (edge-assoc edge) key))

(define-public (edge-save! context edge)
  (call-with-cursor context 'edge
    (lambda (cursor)
      (cursor-update* cursor
                      (list (edge-uid edge))
                      (list (edge-start edge)
                            (edge-label edge)
                            (edge-end edge)
                            (scm->string (edge-assoc edge))))))
  (call-with-cursor context 'edge-index
    (lambda (cursor)
      (index-update! cursor (edge-uid edge) (edge-initial-assoc edge) (edge-assoc edge)))))

(define-public (edge-with-label context label)
  (let ((cursor (context-ref context 'edge-label)))
    (map cadr (cursor-range cursor label))))

;;;
;;; tests
;;;

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_GRF"))
  (format #t "* Testing grf\n")

  (test-check "wiredtigerz"
    (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/wt" *grf*))
      (with-cnx cnx
        #true))
    #true)

  (test-check "vertex-add!"
    (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/wt" *grf*))
      (with-cnx cnx
        (vertex-uid (vertex-add! context "test" '((a . 42))))))
        1)

  (test-check "vertex-ref"
    (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/wt" *grf*))
      (let ((vertex (vertex-add! context "test" '((a . 42)))))
        (with-cnx cnx
          (vertex-assoc (vertex-ref context (vertex-uid vertex))))))
      '((a . 42)))

  (test-check "vertex-save!"
    (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/wt" *grf*))
      (let* ((vertex (vertex-add! context "test" '((a . 42))))
             (vertex (vertex-assoc-set vertex 'a 666)))
        (vertex-save! context vertex)
        (with-cnx cnx
          (vertex-assoc-ref (vertex-ref context (vertex-uid vertex)) 'a))))
      666)
  )

