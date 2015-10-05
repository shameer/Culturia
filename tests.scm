#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64))  ;; unit test framework
(use-modules (srfi srfi-41))  ;; stream

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
    (mkdir path)
    e ...
    (rmtree path)))


(test-group "culturia"

  (with-directory "/tmp/culturia"
                  (let* ((culturia (open-culturia "/tmp/culturia"))
                         (atom (create-atom culturia (list (cons "name" "Kely Vaue")))))

                    ;; a few sanity tests
                    (test-equal "empty outgoings"
                      (stream->list (atom-outgoings atom))
                      (list))

                    (test-equal "empty incomings"
                      (stream->list (atom-incomings atom))
                      (list))

                    (test-equal "assoc" (atom-assoc atom) (list (cons "name" "Kely Vaue")))

                    (let ((idem (culturia-ref culturia (atom-uid atom))))
                      (test-equal "atom-assoc is equal" (atom-assoc atom) (atom-assoc idem)))

                    ;;
                    (let ((atom! (atom-set atom "name" "Vaue Kely")))
                      (test-equal "atom-set" (atom-ref atom! "name") "Vaue Kely")


                      (let ((other (create-atom culturia (list (cons "name" "Oter Ahtom")))))
                        ;; prepare
                        (atom-link atom other)
                        
                        ;; tests
                        (test-equal "not empty outgoings"
                        (stream->list (atom-outgoings atom)) (list other))
                        
                        (test-equal "not empty incomings"
                                    (stream->list (atom-incomings other)) (list atom!))))

                    
                    (culturia-close culturia)
                    )
                  ))


(test-end "main")
