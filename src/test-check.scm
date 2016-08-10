(define-module (test-check))

(use-modules (path))


(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (with-directory "/tmp/wt"
                     (format #t "** Checking ~a\n" title)
                     (let* ((expected expected-result)
                            (produced tested-expression))
                       (if (not (equal? expected produced))
                           (begin (format #t "*** Expected: ~s\n" expected)
                                  (format #t "*** Computed: ~s\n" produced))))))))

(export test-check)
