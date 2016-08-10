;; grf.scm

;; Copyright © 2014-2016 Amirouche BOUBEKKI <amirouche@hypermove.net>

;; grf is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.

;; grf is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with grf.  If not, see <http://www.gnu.org/licenses/>
(define-module (grf))

(use-modules (srfi srfi-1))  ;; lists
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-field
(use-modules (srfi srfi-41))  ;; streams

;; (use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 match))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))

;;;
;;; plain records
;;;
;;
;; macro to quickly define immutable records
;;
;;
;; Usage:
;;
;;   (define-record-type <car> seats wheels)
;;   (define smart (make-car 2 4))
;;   (car-seats smart) ;; => 2
;;
;; Mutation is *not* done in place, via set-field or set-fields eg.:
;;
;; (define smart-for-4 (set-field smart (seats) 4))
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

;;;
;;; Grf
;;;

(define-public *grf* '((vertex
                                ((uid . record))
                                ((label . string)
                                 (assoc . string))
                                ((label (label) (uid))))
                               (edge
                                ((uid . record))
                                ((start . unsigned-integer)
                                 (label . string)
                                 (end . unsigned-integer)
                                 (assoc . string))
                                ((label (label) (uid))
                                 (outgoings (start) (uid))
                                 (incomings (end) (uid))))))

;;;
;;; Vertex
;;;

(define-record-type* <vertex> uid label assoc)

(export vertex-uid vertex-assoc)

(set-record-type-printer! <vertex>
                          (lambda (record port)
                            (format port
                                    "<vertex uid: ~a label: ~a>"
                                    (vertex-uid record)
                                    (vertex-label record))))

(define-public (vertex-ref context uid)
  (let ((cursor (context-ref context 'vertex)))
    (match (cursor-value-ref* cursor uid)
      (() (throw 'vertex-not-found uid))
      ((label assoc) (make-vertex uid label (string->scm assoc))))))

(define-public (vertex-label? edge label)
  (equal? (vertex-label edge) label))

(define-public (vertex-add! context label assoc)
  (let ((cursor (context-ref context 'vertex-append)))
    (cursor-insert* cursor #nil (list label (scm->string assoc)))))

(define-public (vertex-assoc-update vertex key value)
  (let* ((assoc (vertex-assoc vertex))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field vertex (vertex-assoc) assoc)))

(define-public (vertex-assoc-ref vertex key)
  (assoc-ref (vertex-assoc vertex) key))

(define-public (vertex-save! context vertex)
  (let ((cursor (context-ref context 'vertex)))
    (cursor-update* cursor
                    (list (vertex-uid vertex))
                    (list (vertex-label vertex) (scm->string (vertex-assoc vertex))))))

(define-public (vertex-outgoings context uid)
  (let ((cursor (context-ref context 'edge-outgoings)))
    (map car (cursor-range cursor uid))))

(define-public (vertex-incomings context uid)
  (let ((cursor (context-ref context 'edge-incomings)))
    (map car (cursor-range cursor uid))))

(define-public (vertex-with-label context label)
  (let ((cursor (context-ref context 'vertex-label)))
    (map cadr (cursor-range cursor label))))

;;;
;;; Edge
;;;

(define-record-type* <edge> uid start label end assoc)

(export edge-uid edge-start edge-label edge-end edge-assoc)

(set-record-type-printer! <edge>
                          (lambda (record port)
                            (format port
                                    "<edge uid: ~a start: ~a label: ~a end: ~a>"
                                    (edge-uid record)
                                    (edge-start record)
                                    (edge-label record)
                                    (edge-end record))))

(define-public (edge-ref context uid)
  (let ((cursor (context-ref context 'edge)))
    (match (cursor-value-ref* cursor uid)
      (() (throw 'edge-not-found uid))
      ((start label end assoc) (make-edge uid start label end (string->scm assoc))))))

(define-public (edge-label? edge label)
  (equal? (edge-label edge) label))

(define-public (edge-add! context start label end assoc)
  (let ((cursor (context-ref context 'edge-append)))
    (cursor-insert* cursor #nil (list start label end (scm->string assoc)))))

(define-public (edge-assoc-update edge key value)
  (let* ((assoc (edge-assoc edge))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field edge (edge-assoc) assoc)))

(define-public (edge-assoc-ref edge key)
  (assoc-ref (edge-assoc edge) key))

(define-public (edge-save! context edge)
  (let ((cursor (context-ref context 'edge)))
    (cursor-update* cursor
                    (list (edge-uid edge))
                    (list (edge-start edge)
                          (edge-label edge)
                          (edge-end edge)
                          (scm->string (edge-assoc edge))))))

(define-public (edge-with-label context label)
  (let ((cursor (context-ref context 'edge-label)))
    (map cadr (cursor-range cursor label))))

;;;
;;; test-check
;;;

(use-modules (ice-9 receive))


(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (format #t "** Checking ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (not (equal? expected produced))
             (begin (format #t "*** Expected: ~s\n" expected)
                    (format #t "*** Computed: ~s\n" produced))))))))


(define (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be prepend"
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


(when (or (getenv "CHECK") (getenv "CHECK_GRF"))
  (format #t "* Testing grf\n")
  (with-directory "/tmp/grf"
                  (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/grf" *grf*))
                    (test-check "wiredtiger-open*"
                                cnx
                                cnx)
                    (connection-close cnx)))

  ;;; basic vertex

  (with-directory "/tmp/grf"
                  (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/grf" *grf*))
                    (test-check "vertex-add!"
                                (vertex-add! context "test" 42)
                                1)
                    (connection-close cnx)))

  (with-directory "/tmp/grf"
                  (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/grf" *grf*))
                    (test-check "vertex-ref"
                                (let ((uid (vertex-add! context "test" 42)))
                                  (vertex-assoc (vertex-ref context uid)))
                                  42)
                    (connection-close cnx)))

  (with-directory "/tmp/grf"
                  (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/grf" *grf*))
                    (test-check "vertex-save!"
                                (let* ((uid (vertex-add! context "test" 42))
                                       (vertex (set-field (vertex-ref context uid) (vertex-assoc) 666)))
                                  (vertex-save! context vertex)
                                  (vertex-assoc (vertex-ref context uid)))
                                666)
                    (connection-close cnx)))

  ;;; do it all test

  (with-directory "/tmp/grf"
                  (receive (cnx context) (apply wiredtiger-open* (cons "/tmp/grf" *grf*))
                    (test-check "do it all!"
                                (let* ((start (vertex-add! context "test" "start"))
                                       (end (vertex-add! context "test" "end"))
                                       (uid (edge-add! context start "test" end 2006))
                                       (uid (edge-add! context end "test" start 3600)))
                                  (append (vertex-incomings context start)
                                          (vertex-outgoings context end)))
                                '(2 2))
                    (connection-close cnx))))
