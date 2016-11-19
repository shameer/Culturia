(define-module (dot))

(use-modules (srfi srfi-69))

(use-modules (ice-9 q))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (ukv))
(use-modules (grf3))


;; sanitize names of queue procedure
(define make-queue make-q)
(define enqueue! enq!)
(define dequeue! deq!)
(define queue-empty? q-empty?)



(define-public (traverse proc root)
  (let ((queue (make-queue))
        (visited (make-hash-table)))

    (hash-table-set! visited root #true)
    (enqueue! queue root)

    (proc root)

    (while (not (queue-empty? queue))
      (let ((current (dequeue! queue)))
        (let next ((adjacents (map (lambda (uid) (edge-end (get uid))) (outgoings current))))
          (unless (null? adjacents)
            (let ((adjacent (car adjacents)))
              (unless (hash-table-ref visited adjacent (lambda () #false))
                (proc adjacent)
                (hash-table-set! visited adjacent #true)
                (enqueue! queue adjacent)
                (next (cdr adjacents))))))))))

(define %colors
  ;; See colortbl.h in Graphviz.
  #("red" "magenta" "blue" "cyan3" "darkseagreen"
    "peachpuff4" "darkviolet" "dimgrey" "darkgoldenrod"))

(define (pop-color hint)
  "Return a Graphviz color based on HINT, an arbitrary object."
  (let ((index (hash hint (vector-length %colors))))
    (vector-ref %colors index)))

(define (emit-prologue name port)
  (format port "digraph \"Guix ~a\" {\n" name))

(define (emit-epilogue port) 
  (display "\n}\n" port))

(define (emit-node id label port)
  (format port "  \"~a\" [label = \"~a\", shape = box, fontname = Helvetica];~%"
          id label))

(define (emit-edge id1 id2 port)
  (format port "  \"~a\" -> \"~a\" [color = ~a];~%"
          id1 id2 (pop-color id1)))

(define (dot vertex-uid)
  (let ((vertex (get vertex-uid)))
    (emit-node vertex-uid (vertex-ref vertex 'name) (current-output-port))
    (for-each (lambda (edge-uid)
                (let ((edge (get edge-uid)))
                  (emit-edge (edge-start edge) (edge-end edge) (current-output-port))))
              (outgoings vertex-uid))))

(emit-prologue "Graph" (current-output-port))
   
(with-env (env-open* "data/test/" (list *ukv*))
  (let ((a (create-vertex (acons 'name 'a '())))
        (b (create-vertex (acons 'name 'b '()))))
    (create-edge a b '())
    (traverse dot (vertex-uid a))))

(emit-epilogue (current-output-port))
