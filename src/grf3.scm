;; Copyright © 2016 Amirouche BOUBEKKI <amirouche@hypermove.net>
(define-module (grf3))

(use-modules (plain))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (ukv))


(define-record-type* <vertex> uid assoc)

(export vertex-assoc vertex-uid)

(define-record-type* <edge> uid assoc)

(export edge-uid edge-assoc)

(define %VERTEX 0)
(define %EDGE 1)

(define-public (grf-create-vertex assoc)
  (let ((assoc (acons '%kind %VERTEX assoc)))
    (let ((uid (ukv-add! assoc)))
      (make-vertex uid assoc))))

(define-public (vertex-set vertex key value)
  (let* ((assoc (vertex-assoc vertex))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field vertex (vertex-assoc) assoc)))

(define-public (vertex-ref vertex key)
  (assoc-ref (vertex-assoc vertex) key))

(define-public (grf-create-edge start end assoc)
  (let* ((assoc (acons '%kind %EDGE assoc))
         (assoc (acons '%start (vertex-uid start) assoc))
         (assoc (acons '%end (vertex-uid end) assoc)))
    (let ((uid (ukv-add! assoc)))
      (make-edge uid assoc))))

(define-public (edge-start edge)
  (assoc-ref (edge-assoc edge) '%start))

(define-public (edge-end edge)
  (assoc-ref (edge-assoc edge) '%end))

(define-public (edge-set edge key value)
  (let* ((assoc (edge-assoc edge))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field edge (edge-assoc) assoc)))

(define-public (edge-ref edge key)
  (assoc-ref (edge-assoc edge) key))

(define-public (grf-save vertex-or-edge)
  (let ((uid (if (vertex? vertex-or-edge) (vertex-uid vertex-or-edge) (edge-uid vertex-or-edge)))
        (assoc (if (vertex? vertex-or-edge) (vertex-assoc vertex-or-edge) (edge-assoc vertex-or-edge))))
    (ukv-update! uid assoc)))
