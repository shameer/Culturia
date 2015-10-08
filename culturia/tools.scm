(define-module (tools))

(define (print . rest)
  (let ((template (reverse (cdr (reverse rest))))
        (parameters (car (reverse rest))))
    (let loop ((template template)
               (parameters parameters))
      (if (null? template)
          (newline)
          (if (procedure? (car template))
              (begin ((car template) (car parameters))
                     (loop (cdr template) (cdr parameters)))
              (begin (display (car template))
                     (loop (cdr template) parameters)))))))

(define (~s s) (format #true "~s" s))

(print  "Héllo World, " ~s (list "you-name-it"))
