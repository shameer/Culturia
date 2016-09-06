;; Copyright © 2016 Amirouche BOUBEKKI <amirouche@hypermove.net>
(define-module (grf3))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-41))

(use-modules (ice-9 match))
(use-modules (ice-9 receive))

(use-modules (plain))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (ukv))


(define-record-type* <vertex> uid assoc)

(export vertex-assoc vertex-uid)

(define-record-type* <edge> uid assoc)

(export edge-uid edge-assoc)

(define-public VERTEX 0)
(define-public EDGE 1)

(define-public (get uid)
  (let ((assoc (ukv-ref* uid)))
    (if (eq? (assoc-ref assoc '%kind) VERTEX)
        (make-vertex uid assoc)
        (make-edge uid assoc))))

(define-public (create-vertex assoc)
  (let ((assoc (acons '%kind VERTEX assoc)))
    (let ((uid (ukv-add! assoc)))
      (make-vertex uid assoc))))

(define-public (vertex-set vertex key value)
  (let* ((assoc (vertex-assoc vertex))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field vertex (vertex-assoc) assoc)))

(define-public (vertex-ref vertex key)
  (assoc-ref (vertex-assoc vertex) key))

(define-public (create-edge start end assoc)
  (let* ((assoc (acons '%kind EDGE assoc))
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

(define-public (save vertex-or-edge)
  (let ((uid (if (vertex? vertex-or-edge) (vertex-uid vertex-or-edge) (edge-uid vertex-or-edge)))
        (assoc (if (vertex? vertex-or-edge) (vertex-assoc vertex-or-edge) (edge-assoc vertex-or-edge))))
    (ukv-update! uid assoc))
  vertex-or-edge)

;;; traversi streams

(define-public (list->traversi lst)
  (let loop ((lst lst))
    (lambda ()
      (if (null? lst)
          '()
          (cons (cons (car lst) '()) (loop (cdr lst)))))))

(define-public (traversi->list traversi)
  (let loop ((traversi traversi)
             (out '()))
    (match (traversi)
      ('() (reverse out))
      ((item . next) (loop next (cons (car item) out))))))

(define-public (traversi-map proc traversi)
  (let loop ((traversi traversi))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next) (cons (cons (proc (car item)) item) (loop next)))))))

(define-public (traversi-filter proc traversi)
  (let loop1 ((traversi traversi))
    (lambda ()
      (let loop2 ((traversi traversi))
        (match (traversi)
          ('() '())
          ((item . next) (if (proc (car item))
                             (cons item (loop1 next))
                             (loop2 next))))))))

(define-public (traversi-parent traversi)
  (let loop ((traversi traversi))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next)
         (let ((parents (cdr item)))
           (if (null? parents)
               (throw 'traversi "item has no parent")
               (cons parents (loop next)))))))))

(define-public (traversi-take count traversi)
  (let loop ((traversi traversi)
             (count count))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next) (cons item (loop next (1- count))))))))

(define-public (traversi-drop count traversi)
  (let loop ((traversi traversi)
              (count count))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next) (if (eq? count 0)
                           (cons item (loop next 0))
                           ((loop next (1- count)))))))))
           

(define-public (traversi-paginator count traversi)
  (throw 'grf3 "not implemented"))

(define-public (traversi-length traversi)
  (let loop ((traversi traversi)
             (count 0))
    (match (traversi)
      ('() count)
      ((item . next) (loop next (1+ count))))))

(define-public (traversi-scatter traversi)
  "take a traversi of lists and returns a traversi made of all the
   elements of all the lists. parents are inherited."
  (let loop1 ((traversi traversi))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next) (let loop2 ((lst item))
                         (lambda ()
                           (if (null? lst)
                               (loop1 next)
                               (cons (cons (car lst) item) (loop2 (cdr lst)))))))))))
;;; traversi helpers

(define-public (vertices)
  (list->traversi (ukv-index-ref '%kind VERTEX)))

(define-public (edges)
  (list->traversi (ukv-index-ref '%kind EDGE)))

(define-public (from key value)
  (list->traversi (ukv-index-ref key value)))

(define-public (where? key value)
  (lambda (uid)
    (equal? (ukv-ref uid key) value)))

(define-public (key name)
  (lambda (uid)
    (ukv-ref uid name)))

(define-public incomings
  (lambda (uid)
    (ukv-index-ref '%end uid)))

(define-public outgoings
  (lambda (uid)
    (ukv-index-ref '%start uid)))

(define-public start
  (lambda (uid)
    (ukv-ref uid '%start)))

(define-public end
  (lambda (uid)
    (ukv-ref uid '%end)))

;;; other helpers

(define-public (get-or-create-vertex key value)
  (let ((uids (traversi->list (traversi-filter (where? '%kind VERTEX) (from key value)))))
    (if (null? uids)
        (values #true (create-vertex (acons key value '())))
        (values #false (get (car uids))))))

;;; tests

(use-modules (test-check))


(when (or (getenv "CHECK") (getenv "CHECK_GRF3"))
  (format #t "* Testing grf3\n")

  (test-check "open database"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      42)
    42)

  (test-check "create vertex"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let ((vertex (create-vertex '((a . 42)))))
        (vertex-ref (get (vertex-uid vertex)) 'a)))
    42)

  (test-check "create edge"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let* ((start (create-vertex '()))
             (end (create-vertex '()))
             (edge (create-edge start end '((a . 42)))))
        (edge-ref (get (edge-uid edge)) 'a)))
    42)

  (test-check "get-or-create-vertex new true"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new vertex) (get-or-create-vertex 'a 42)
        new))
    #true)

  (test-check "get-or-create-vertex new false"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new vertex) (get-or-create-vertex 'a 42)
        (receive (new vertex) (get-or-create-vertex 'a 42)
          new)))
    #false)

  (test-check "save vertex"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let* ((a (create-vertex '((a . 42))))
             (a (vertex-set a 'b 1337)))
        (save a)
        (vertex-ref (get (vertex-uid a)) 'b)))
    1337)

  (test-check "save edge"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let* ((start (create-vertex '()))
             (end (create-vertex '()))
             (edge (create-edge start end '((a . 42)))))
        (save (edge-set edge 'b 1337))
        (edge-ref (get (edge-uid edge)) 'b)))
    1337)

  )
