(define-module (cli))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))  ;; date
(use-modules (srfi srfi-26))  ;; cut
(use-modules (srfi srfi-41))  ;; stream

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))

(use-modules (srfi-99))


(define-record-type* <command>
  name
  help
  subs
  argument
  options
  lambda)


(define (command-parse-args rest command)
  (if (null? rest)
      command
      (command-parse-args (cdr rest)
                          (cond
                           ((command? (car rest))
                            (let ((subs (cons (car rest) (command-subs command))))
                              (set-field command (command-subs) subs)))
                           ((procedure? (car rest))
                            (set-field command (command-lambda) (car rest)))
                           ((string? (car rest))
                            (set-field command (command-argument) (car rest)))
                           (else (set-field command
                                            (command-options)
                                            (cons (car rest)
                                                  (command-options command))))))))


(define keyword->string (compose symbol->string keyword->symbol))


(define (command-parse-options options)
  (let loop ((options (reverse options))
             (assoc (list)))
    (if (null? options)
        assoc
        (loop (cddr options)
              (acons (keyword->string (car options)) (cadr options) assoc)))))


(define-public (command name help . rest)
  (let* ((command (command-parse-args rest (make-command name help (list) #nil #nil (list))))
         (options (command-parse-options (command-options command))))
    (set-field command (command-options) options)))


(define (print . rest)
  (let loop ((msg rest))
    (if (not (null? msg))
        (begin (display (car msg)) (display " ")
               (loop (cdr msg)))))
  (newline))


(define (help pre command)
  (define prefix (string-append pre " " (command-name command)))
  (print (string-append prefix ":") (command-help command))
  (let loop ((cmds (command-subs command)))
    (when (not (null? cmds))
      (help prefix (car cmds))
      (loop (cdr cmds)))))


(define (strip-quote string)
  (if (string-prefix? "\"" string)
      (substring string 1 (- (string-length string) 1))
      string))


(define (make-flag option)
  (match (string-split (string-drop option 2) #\=)
    ((name value) (cons name (strip-quote value)))
    ((name) (cons name #true))))


(define (parse-options options)
  (let next ((options options)
             (flags (list))
             (arguments (list)))
    (if (null? options)
        (list flags arguments)
        (if (string-prefix? "--" (car options))
            (next (cdr options) (cons (make-flag (car options)) flags) arguments)
            (next (cdr options) flags (cons (car options) arguments))))))

;; (parse-options (list "--foo" "--bar" "baz" "boom" "--blim=\"spam egg\""))


(define (program-arguments-parse arguments command)
  (let loop ((arguments (cdr arguments))
             (command command))
    (if (or (null? arguments) (string-prefix? "--" (car arguments)))
        (values command (parse-options arguments))
        (loop (cdr arguments)
              (let next ((name (car arguments))
                         (commands (command-subs command)))
                (if (null? commands)
                    (print "ERROR: no such command")
                    (if (equal? name (command-name (car commands)))
                        (car commands)
                        (next name (cdr commands)))))))))


(define-public (program-execute program command)
  (receive (command options) (program-arguments-parse program command)
    (if (null? (command-lambda command))
        (help "" command)
        (apply (command-lambda command) options))))
