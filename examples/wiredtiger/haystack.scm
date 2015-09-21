(define-module (haystack))

(use-modules (srfi srfi-1))  ;; fold
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-26))  ;; cut

(use-modules (ice-9 match))  ;; match

(use-modules (wiredtiger))  ;; key/value store

;;;
;;; Guile helpers
;;;
;;
;; macro to quickly define immutable records
;;
;; FIXME: Taken from Guile (maybe should be in (srfi srfi-99))
;;        adapted to make it possible to declare record type like `<abc>' and
;;        keep field accessor bracket free. record name *must* have brackets or
;;        everything is broken
;;
;; Usage:
;;
;;   (define-record-type <abc> field-one field-two)
;;   (define zzz (make-abc 1 2))
;;   (abc-field-one zzz) ;; => 1
;;
;; FIXME: maybe this is less useful than the immutable record
;;        of (srfi srfi-9 gnu)
;;        I use `set-field` and `set-fields`
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

;;; database

(define-record-type* <database> connection session)

(define (connect)
  (let* ((connection (connection-open "haystack.db" "create"))
         (session (session-open connection)))
    (session-create session "table:documents" "key_format=r,value_format=S")
    (session-create session "table:grams" "key_format=Si,value_format=S")
    (make-database connection session)))

(define (database-close database)
  (session-close (database-session database))
  (connection-close (database-connection database)))

;;; commands

(define (lst)
  (let* ((database (connect))
         (cursor (cursor-open (database-session database)
                              "table:documents" "raw,")))
    (let loop ()
      (if (cursor-next cursor)
          (begin
            (format #true
                    "~a: ~a\n"
                    (car (cursor-key-ref cursor))
                    (car (cursor-value-ref cursor)))
            (loop))))
    (database-close database)))

(define (%text->grams% text)
  (define (word->grams word)
    (let loop ((word word)
               (grams '()))
      (if (>= (string-length word) 3)
          (loop (string-drop word 1) (cons (string-take word 3) grams))
          ;; do not index grams < 3
          (reverse grams))))

  (define words (filter (lambda (word) (not (equal? word "")))
                        (string-split text #\space)))

  (fold (lambda (word grams) (append grams (word->grams word))) '() words))

(define (index title text)
  (let* ((database (connect))
         (documents (cursor-open (database-session database)
                                 "table:documents"
                                 "append")))
    ;; insert new document
    (cursor-value-set documents title)
    (cursor-insert documents)
    ;; insert grams
    (let ((grams (cursor-open (database-session database) "table:grams"))
          (identifier (car (cursor-key-ref documents))))
      (map (lambda (gram)
             (cursor-key-set grams gram identifier)
             (cursor-value-set grams "ok")
             (cursor-insert grams))
           (%text->grams% text))
      ;; cleanup
      (cursor-close grams))
    ;; cleanup
    (cursor-close documents)
    (database-close database)))

(define (uniquify input)
  "INPUT must be sorted list"
  (let loop ((lst (cdr input))
             (out (list (car input))))
    (if (null? lst)
        (reverse out)
        (if (equal? (car lst) (car out))
            (loop (cdr lst) out)
            (loop (cdr lst) (cons (car lst) out))))))

(define (count lst)
  (define counter (make-hash-table))
  (for-each (lambda (item)
              (if (hash-ref counter item)
                  (hash-set! counter item (1+ (hash-ref counter item) ))
                  (hash-set! counter item 1)))
            lst)
  counter)


(define (make-sort counter)
  (lambda (a b)
    (< (hash-ref counter a) (hash-ref counter b))))

(define (search keywords)
  (let* ((database (connect))
         (documents (cursor-open (database-session database)
                                 "table:documents"))
         (grams (cursor-open (database-session database) "table:grams"))
         ;; retrieve identifiers of the records
         ;; where the grams of keywords appear
         (identifiers (fold (lambda (gram identifiers)
                              (cursor-key-set grams gram 0)
                              (if (cursor-search-near grams)                      
                                  (let loop ((identifiers identifiers))
                                    (if (cursor-next grams)
                                        (match (cursor-key-ref grams)
                                          [(other identifier)
                                           (if (equal? gram other)
                                               (loop (cons identifier identifiers))
                                   identifiers)])
                                        identifiers))))
                            '()
                            (%text->grams% keywords)))
         (counter (count identifiers))
         (identifiers (uniquify (sort identifiers (make-sort counter)))))
    (for-each (lambda (identifier)
                (cursor-key-set documents identifier)
                (cursor-search documents)
                (format #true
                        "~a: ~a\n"
                        identifier
                        (car (cursor-value-ref documents))))
              identifiers)
    ;; cleanup
    (cursor-close grams)
    (cursor-close documents)
    (database-close database)))

(define (debug)
  (let* ((database (connect))
         (grams (cursor-open (database-session database) "table:grams" "raw")))
    (let next ()
      (if (cursor-next grams)
          (begin
            (format #true "index record (gram identifier): ~s\n" (cursor-key-ref grams))
            (next))))
    (cursor-close grams)
    (database-close database)))

(define (main args)
  (match args
    (("list") (lst))
    (("index" title value) (index title value))
    (("search" keywords) (search keywords))
    (("debug") (debug))
    (_ (begin (display "Welcome haystack.scm a tool that will allow you to index stuff using trigrams\n")
              (newline)
              (display "\tlist \t\t\tList everything in the index\n")
              (display "\tindex TITLE VALUE\tAdd something to the index\n")
              (display "\tsearch KEYWORDS\t\tSearch the index\n")
              (display "\tdebug\t\t\tPrint the content of the index\n")))))


(main (cdr (command-line)))
