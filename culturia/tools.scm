(define-module (tools))

(define-public (print . rest)
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

(define-public (~s s) (format #true "~s" s))
(define-public (~a s) (format #true "~a" s))

;; (print  "Héllo World, " ~s (list "you-name-it"))



(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (print "* Checking " ~s (list title))
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (not (equal? expected produced))
             (begin (print "Expected: " ~a (list expected))
                    (print "Computed: " ~a (list produced)))))))))

(export test-check)
