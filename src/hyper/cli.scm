(define-module (cli))

(use-modules (ice-9 match))


(define (strip-quote string)
  (if (string-prefix? "\"" string)
      (substring string 1 (- (string-length string) 1))
      string))

(define (make-flag option)
  (match (string-split (string-drop option 2) #\=)
    ((name value) (cons name (strip-quote value)))
    ((name) (cons name #true))))

(define-public (parse-arguments args)
  (let loop ((args args)
             (flags '())
             (extra '()))
    (if (null? args)
        (values flags extra)
        (let ((arg (car args)))
          (if (string-prefix? "--" arg)
              (loop (cdr args) (cons (make-flag arg) flags) extra)
              (loop (cdr args) flags (cons arg extra)))))))

;;; test-check

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin 
       (format #t "** Checking ~a\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (not (equal? expected produced))
             (begin (format #t "*** Expected: ~s\n" expected)
                    (format #t "*** Computed: ~s\n" produced))))))))

(use-modules (ice-9 receive))

(define-syntax-rule (values->list a)
  (receive args a args))

(when (or (getenv "CHECK") (getenv "CHECK_CLI"))
  (format #t "* Testing cli\n")

  (test-check "no arguments"
    (values->list (parse-arguments '()))
    '(() ()))
  
  (test-check "single boolean option"
    (values->list (parse-arguments '("--flag")))
    '((("flag" . #t)) ()))

  (test-check "single extra argument"
    (values->list (parse-arguments '("argument")))
    '(() ("argument")))
  
  (test-check "single option with value"
    (values->list (parse-arguments '("--flag=value")))
    '((("flag" . "value")) ()))

  (test-check "single option with value with one extra argument"
    (values->list (parse-arguments '("--flag=value" "argument")))
    '((("flag" . "value")) ("argument")))

  (test-check "do it all!"
    (values->list (parse-arguments '("--flag" "--key=value" "arg0" "arg1")))
    '((("key" . "value") ("flag" . #t)) ("arg1" "arg0")))

  (test-check "do it all without classic ordering"
    (values->list (parse-arguments '("arg0" "--flag" "arg1" "--key=value")))
    '((("key" . "value") ("flag" . #t)) ("arg1" "arg0"))))
