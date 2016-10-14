(define-module (html2text))

(use-modules (ice-9 optargs))


(define (element->string tag attr elements port)
  (cond
   ((eq? tag 'script) *unspecified*)
   ((eq? tag 'head) *unspecified*)
   (else (map (lambda (element) (html->text element port)) elements))))

(define*-public (html->text tree #:optional (port (current-output-port)))
  (cond
   ((pair? tree)
    (if (symbol? (car tree))
        ;; An element.
        (let ((tag (car tree)))
          (case tag
            ((*TOP*)
             (html->text (cdr tree) port))
            ((*ENTITY*) *unspecified*)
            ((*PI*) *unspecified*)
            (else
             (let* ((elems (cdr tree))
                    (attrs (and (pair? elems) (pair? (car elems))
                                (eq? '@ (caar elems))
                                (cdar elems))))
               (element->string tag attrs (if attrs (cdr elems) elems) port)))))
        ;; A nodelist.
        (for-each (lambda (x) (html->text x port)) tree)))
   ((string? tree)
    (display tree port) (display " " port))
   ((null? tree) *unspecified*)
   ((not tree) *unspecified*)
   ((eqv? tree #t) *unspecified*)
   ((procedure? tree)
    (with-output-to-port port tree))
   (else
    (display tree port))))
