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
(use-modules (plain))
(use-modules (uav))

;;; Vertex

(define-record-type* <vertex> uid assoc)

(export vertex-uid vertex-assoc)

(set-record-type-printer! <vertex>
                          (lambda (record port)
                            (format port
                                    "<vertex uid: ~a label: ~a>"
                                    (vertex-uid record)
                                    (vertex-label record))))

(define-public (vertex-ref uid)
  (let ((assoc (uav-ref* uid)))
    (if (null? assoc)
        (throw 'vertex-not-found uid)
        (make-vertex uid assoc))))

(define-public (vertex-label vertex)
  (assoc-ref (vertex-assoc vertex) 'vertex/label))

(define-public (vertex-label? edge label)
  (equal? (vertex-label edge) label))

(define-public (vertex-add! label assoc)
  (let* ((assoc (acons 'vertex/label label assoc))
         (uid (uav-add! assoc)))
    (make-vertex uid assoc)))

(define-public (vertex-assoc-set vertex key value)
  (let* ((assoc (vertex-assoc vertex))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field vertex (vertex-assoc) assoc)))

(define-public (vertex-assoc-ref vertex key)
  (assoc-ref (vertex-assoc vertex) key))

(define-public (vertex-save! vertex)
  (uav-update! (vertex-uid vertex) (vertex-assoc vertex)))

(define-public (vertex-outgoings vertex)
  (uav-index-ref 'edge/start (vertex-uid vertex)))
  
(define-public (vertex-incomings vertex)
  (uav-index-ref 'edge/end (vertex-uid vertex)))

(define-public (vertex-with-label label)
  (uav-index-ref 'vertex/label label))

(define-public (vertex-get-or-create attribute label assoc)
  (match (uav-index-ref attribute (assoc-ref assoc attribute))
    (() (values #true (vertex-add! label assoc)))
    ((uid) (values #false (vertex-ref uid)))
    (_ (throw 'database-is-broken))))

;;; Edge

(define-record-type* <edge> uid assoc)

(export edge-uid edge-assoc)

(set-record-type-printer! <edge>
                          (lambda (record port)
                            (format port
                                    "<edge uid: ~a start: ~a label: ~a end: ~a>"
                                    (edge-uid record)
                                    (edge-start record)
                                    (edge-label record)
                                    (edge-end record))))

(define-public (edge-label edge)
  (assoc-ref (edge-assoc edge) 'edge/label))

(define-public (edge-start edge)
  (vertex-ref (assoc-ref (edge-assoc edge) 'edge/start)))

(define-public (edge-end edge)
  (vertex-ref (assoc-ref (edge-assoc edge) 'edge/end)))

(define-public (edge-ref uid)
  (let ((assoc (uav-ref* uid)))
    (if (null? assoc)
        (throw 'edge-not-found uid)
        (make-edge uid assoc))))

(define-public (edge-label? edge label)
  (equal? (edge-label edge) label))

(define-public (edge-add! start label end assoc)
  (let* ((assoc (acons 'edge/label label assoc))
         (assoc (acons 'edge/start (vertex-uid start) assoc))
         (assoc (acons 'edge/end (vertex-uid end) assoc))
         (uid (uav-add! assoc)))
    (make-edge uid assoc)))

(define-public (edge-assoc-update edge key value)
  (let* ((assoc (edge-assoc edge))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field edge (edge-assoc) assoc)))

(define-public (edge-assoc-ref edge key)
  (assoc-ref (edge-assoc edge) key))

(define-public (edge-save! edge)
  (uav-update! (edge-uid edge) (edge-assoc edge)))

(define-public (edge-with-label label)
  (uav-index-ref 'edge/label label))


;;; tests

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_GRF"))
  (format #t "* Testing grf\n")

  (test-check "uav-open*"
      (with-cnx (uav-open* "/tmp/wt")
        #true)
      #true)

  (test-check "vertex-add!"
    (with-cnx (uav-open* "/tmp/wt")
      (vertex-assoc-ref (vertex-ref (vertex-uid (vertex-add! 'test '((a . 42))))) 'a))
    42)

  (test-check "edge-add!"
    (with-cnx (uav-open* "/tmp/wt")
      (let ((start (vertex-add! 'start '()))
            (end (vertex-add! 'end '())))
        (edge-assoc-ref (edge-ref (edge-uid (edge-add! start 'test end '((a . 42))))) 'a)))
    42)

  (test-check "vertex-outgoings and incomings"
    (with-cnx (uav-open* "/tmp/wt")
      (let ((vertex (vertex-add! 'vertex '())))
        (edge-add! vertex 'test vertex '())
        (apply equal? (list (vertex-outgoings vertex)
                            (vertex-incomings vertex)))))
    #t)
    
  (test-check "edge-start and end"
    (with-cnx (uav-open* "/tmp/wt")
      (let* ((vertex (vertex-add! 'vertex '()))
             (edge (edge-add! vertex 'test vertex '())))
        (apply equal? (list (edge-start edge)
                            (edge-end edge)
                            vertex))))
    #t)

  (test-check "vertex-get-or-create not created"
    (with-cnx (uav-open* "/tmp/wt")
      (let* ((vertex (vertex-add! 'vertex '((a . 42)))))
        (receive (created vertex) (vertex-get-or-create 'a 'vertex '((a . 42)))
          created)))
    #false)
  
  (test-check "vertex-get-or-create created"
    (with-cnx (uav-open* "/tmp/wt")
      (receive (created vertex) (vertex-get-or-create 'a 'vertex '((a . 42)))
        created))
    #true)
  
  )
