#!/usr/bin/env guile
!#

(use-modules (srfi srfi-64)) ;; unit test framework

(use-modules (culturia))


(test-begin "main")

(test-group "utils"
  (test-group "tree"
    (let* ((tree (create-tree 1))
           (_ (tree-append! tree 1 2))
           (_ (tree-append! tree 1 3))
           (_ (tree-append! tree 2 4))
           (_ (tree-append! tree 4 5))
           (_ (tree-append! tree 5 6))
           (_ (tree-append! tree 6 7)))
      (test-equal (tree-path tree 6) (list 6 5 4 2 1))))
  )

;; ---

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
      (rmdir path))
    e ...
    (rmtree path)))


(test-group "culturia"

  (with-directory "/tmp/culturia"
                  (let* ((culturia (create-culturia "/tmp/culturia"))
                         (master (checkout-revision culturia "master")))

                    (pk master)
                    (culturia-close culturia)
  
                  ;; (test-equal "smoke" (list 1 2) (list 1 2))
                    )
                  ))


(test-end "main")
