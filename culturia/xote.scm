(use-modules (rnrs hashtables))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-19))  ;; date
(use-modules (srfi srfi-26))  ;; cut
(use-modules (srfi srfi-41))  ;; stream

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))


;; helper for managing exceptions

(define (make-exception name)
  "Generate a unique symbol prefixed with NAME"
  (gensym (string-append "xote-" name "-")))

(define *exception* (make-exception "exception"))

(define (raise message . rest)
  "shorthand to throw EXCEPTION with MESSAGE formated with REST"
  (throw *exception* (apply format (append (list #false message) rest))))

;; well, i'm too lazy to create other error messages
(define (Oops!)
  (raise "Oops!"))


;; ---

;;;
;;; srfi-99
;;;
;;
;; macro to quickly define immutable records
;;
;;
;; Usage:
;;
;;   (define-record-type <abc> field-one field-two)
;;   (define zzz (make-abc 1 2))
;;   (abc-field-one zzz) ;; => 1
;;

(define-syntax define-record-type*
  (lambda (x)
    (define (%id-name name) (string->symbol (string-drop (string-drop-right (symbol->string name) 1) 1)))
    (define (id-name ctx name)
      (datum->syntax ctx (%id-name (syntax->datum name))))
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ rname field ...)
       (and (identifier? #'rname) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'rname #'make- (id-name #'rname #'rname)))
                     (pred (id-append #'rname (id-name #'rname #'rname) #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f (id-name #'rname #'rname) #'- f))
                                        #'(field ...))))
         #'(define-record-type rname
             (cons field ...)
             pred
             (field getter)
             ...))))))


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


(define (command name help . rest)
  (let* ((command (command-parse-args rest (make-command name help (list) #nil #nil (list))))
         (options (command-parse-options (command-options command))))
    (set-field command (command-options) options)))


(define (command-sub-ref command path)
  (let loop ((path path)
             (command command))
    (if (null? path)
        command
        (loop (cdr path)
              (let next ((subs (command-subs command)))
                (if (null? subs)
                    (Oops!)  ;; command not found
                    (if (equal? (command-name (car subs)) (car path))
                        (car subs)
                        (next (cdr subs)))))))))




(define-record-type* <program> path options argument)


(define (%create-program% args program)
  (if (null? args)
      program
      (%create-program%
       (cdr args)
       (if (string-prefix? "--" (car args))
           ;; add option
           (let* ((options (program-options program))
                  (option (string-drop (car args) 2))
                  (options (acons option #true options)))
             (set-field program (program-options) options))
           ;; add path
           (set-field program (program-path) (cons (car args) (program-path program)))))))


(define (create-program)
  (let ((args (reverse (cdr (program-arguments)))))
    (if (string-prefix? "--" (car args))
        (%create-program% args (make-program (list) (list) #nil))
        (%create-program% (cdr args) (make-program (list) (list) (car args))))))



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


(define xote.scm (command "xote.scm" "alternative guix command line"
                          (command "package" "all package related commands"
                                   (command "download" "download a package" "<package>"
                                            (lambda (package options)
                                              (format #true "* downloading ~s" package)))
                                   (command "search" "search a package" "<package>" #:regex #false
                                            (lambda (package options)
                                              (format #true "* searching for ~s options: ~a" package options))))))


(define (program-execute program command)
  (let ((command (command-sub-ref command (program-path program))))
    ((command-lambda command) (program-argument program) (program-options program))))


(program-execute (create-program) xote.scm)
