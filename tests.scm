#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)) ;; unit test framework

(use-modules (culturia))


(test-begin "main")

;;; path utils

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

(define-syntax-rule (with-directory path e ...)
  (begin
    (when (access? path F_OK)
      (rmtree path))
    e ...
    (rmtree path)))


(test-group "culturia"

  (with-directory "/tmp/culturia"
                  (let* ((culturia (culturia-create "/tmp/culturia"))
                         (atom (culturia-atom-create culturia "test" "name")))
                    ;; a few sanity tests
                    (test-equal "empty outgoings" (atom-outgoings atom) (list))
                    (test-equal "empty incomings" (atom-incomings atom) (list))
                    (test-equal "empty assoc" (atom-assoc atom) (list))

                    (let ((idem (culturia-atom-ref/uid culturia (atom-uid atom))))
                      (test-equal "atom-assoc-equal" (atom-assoc atom) (atom-assoc idem)))

                    (let ((idem (culturia-atom-ref culturia "test" "name")))
                      (test-equal "culturia-atom-ref/type+name" (atom-uid atom) (atom-uid idem)))

                    (let ((atoms (culturia-atom-ref culturia "test")))
                      (test-equal "culturia-atom-ref/type" atoms (list (atom-uid atom))))

                    ;;
                    (atom-assoc-set! atom 'key "value")
                    (test-equal "atom-assoc-set" (atom-assoc-ref atom 'key) "value")

                    (let ((other (culturia-atom-create culturia "test" "other")))
                      (atom-link atom other)
                      (test-equal "not empty outgoings" (atom-outgoings atom) (list (atom-uid other)))
                      (test-equal "not empty incomings" (atom-incomings other) (list (atom-uid atom))))

                    (culturia-close culturia)


                  ;; (test-equal "smoke" (list 1 2) (list 1 2))
                    )
                  ))


(test-end "main")
