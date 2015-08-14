(use-modules (srfi srfi-64))

(use-modules (tpldb))

;;;
;;; helpers
;;;

(define-syntax-rule (match-let (value query) e ...)
  (match query (value e ...)))

;;;
;;; Path helpers
;;;

(define (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be  prepend"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))

(define (path-dfs-walk dirpath proc)
  (define dir (opendir dirpath))
  (let loop ()
    (let ((entry (readdir dir)))
      (cond
       ((eof-object? entry))
       ((or (equal? entry ".") (equal? entry "..")) (loop))
       (else (let ((path (path-join dirpath entry)))
               (if (equal? (stat:type (stat path)) 'directory)
                   (begin (path-dfs-walk path proc)
                          (proc path))
                   (begin (proc path) (loop))))))))
  (closedir dir)
  (proc (path-join dirpath)))

(define (rmtree path)
  (path-dfs-walk path (lambda (path)
                        (if (equal? (stat:type (stat path)) 'directory)
                            (rmdir path)
                            (delete-file path)))))

;;;
;;; tpldb tests
;;;

(define-syntax-rule (with-database proc)
  (begin
    (mkdir "tmpdb")
    (let* ((db (create-tpldb "tmpdb"))
          (out (proc db)))
      (tpldb-close db)
      (rmtree "tmpdb")
      (pk 'out out))))

(test-begin "tpldb")

(test-equal "create database and close it"
  (with-database (lambda (db)
                   #true))
  #true)

(test-begin "tpldb-cursor-map")

(test-equal "unbound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "unbound")
       (tpldb-add db "one" "test/title" "another tuple")
       (tpldb-add db "two" "test/title" "another one"))
     (tpldb-cursor-map (tpldb-iav db) (lambda (key) key) "" "")))
  ;; the ordered is the inverse of lexicographic order
  (list (list "zero" "test/title")
        (list "two" "test/title")
        (list "one" "test/title")))

(test-equal "semi-bound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "semi-bound")
       (tpldb-add db "zero" "test/description" "another tuple")
       (tpldb-add db "one" "test/title" "not selected"))
     (tpldb-cursor-map (tpldb-iav db) (lambda (key) key) "zero" "")))
  (list (list "zero" "test/title") (list "zero" "test/description")))

(test-equal "bound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "bound")
       (tpldb-add db "zero" "test/description" "not selected")
       (tpldb-add db "one" "test/title" "not selected"))
     (tpldb-cursor-map (tpldb-iav db) (lambda (key) key) "zero" "test/title")))
  (list (list "zero" "test/title")))

(test-end)


(test-begin "tpldb-iav-map")

(test-equal "unbound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "unbound")
       (tpldb-add db "one" "test/title" "another tuple"))
     (tpldb-iav-map db (lambda (. rest) rest) "" "")))
  (list (list "zero" "test/title" "unbound")
        (list "one" "test/title" "another tuple")))

(test-equal "semi-bound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "semibound")
       (tpldb-add db "zero" "test/description" "This must match two tuples")
       (tpldb-add db "one" "test/title" "This will not be matched"))
     (tpldb-iav-map db (lambda (. rest) rest) "zero" "")))
  (list (list "zero" "test/title" "semibound")
        (list "zero" "test/description" "This must match two tuples")))

(test-equal "bound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "bound")
       (tpldb-add db "zero" "test/description" "not selected")
       (tpldb-add db "one" "test/title" "not selected"))
     (tpldb-iav-map db (lambda (. rest) rest) "zero" "test/title")))
  (list (list "zero" "test/title" "bound")))

(test-end)


(test-begin "tpldb-avi-map")

(test-equal "unbound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "unbound")
       (tpldb-add db "one" "test/title" "another tuple"))
     (tpldb-avi-map db (lambda (. rest) rest) "")))
  (list (list "test/title" "another tuple" "one")
        (list "test/title" "unbound" "zero")))

(test-equal "semi-bound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "semibound")
       (tpldb-add db "zero" "test/description" "This must match two tuples")
       (tpldb-add db "one" "test/title" "another one"))
     (tpldb-avi-map db (lambda (. rest) rest) "test/title")))
  (list (list "test/title" "another one" "one")
        (list "test/title" "semibound" "zero")))

(test-equal "bound"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "bound")
       (tpldb-add db "zero" "test/description" "not selected")
       (tpldb-add db "one" "test/title" "not selected"))
     (tpldb-avi-map db (lambda (. rest) rest) "test/title" "bound")))
  (list (list "test/title" "bound" "zero")))

(test-end)


(test-begin "tpldb-ref")

(test-equal "create two tuple and retrieve them as an alist"
  (with-database
   (lambda (db)
     (with-transaction db
       (tpldb-add db "zero" "test/title" "create a tuple and retrieve it")
       (tpldb-add db "zero" "test/body" "return all the associated tuples"))
     (tpldb-ref db "zero")))
  (list
   (cons "test/title" "create a tuple and retrieve it")
   (cons "test/body" "return all the associated tuples")))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
