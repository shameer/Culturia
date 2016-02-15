;;   miniKanren
;;
;; Implementation based on microKanren compatible with GNU Guile
;;
;; Forked from https://github.com/amirouche/microKanren
;;
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman
;; Copyright (C) 2015 Amirouche Boubekki
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
(define-module (minikanren))


(define-public (var c) (vector c))
(define-public (var? x) (vector? x))
(define-public var=? equal?)


(define (assp proc alist)
  "Return the first assoc from ALIST for which PROC return TRUE"
  (let next ((alist alist))
    (if (null? alist)
        #false
        (if (proc (caar alist))
            (car alist)
            (next (cdr alist))))))


(define-public (walk u s)
  "Find U substitution in S"
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))


;; (acons x v substitutions)
;; ie. add `v substitue x` to the list of substituations
(define ext-s acons)


(define-public (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit (cons s (cdr s/c))) mzero))))


(define-public (unit s/c) (cons s/c mzero))
(define-public mzero '())


(define-public (unify u v s)
  ;; resolve U and V according to substitution S
  (let ((u (walk u s)) (v (walk v s)))
    ;; unify if possible
    (cond
     ;; U and V are variable and they are the same substitution
     ((and (var? u) (var? v) (var=? u v)) s)
     ;; U is a variable, substitute with V
     ((var? u) (ext-s u v s))
     ;; V is a variable, substitute with U
     ((var? v) (ext-s v u s))
     ;; both U and V are pairs, substitute recursively
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     ;; otherwise, substitutions is ok, if U and V are EQV?
     (else (and (equal? u v) s)))))


(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))


(define-public (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define-public (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (bind ($) g)))
   (else (mplus (g (car $)) (bind (cdr $) g)))))


;;;; How to make a simple minikanren

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(export conj+)

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(export disj+)

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(export fresh)

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(export conde)

;; (define-syntax run
;;   (syntax-rules ()
;;     ((_ n (x ...) g0 g ...)
;;      (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))))

;; (export run)


(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))))

(export run*)

(define empty-state '(() . 0))

(define (call/goal g) (g empty-state))

(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define (take n $)
  (if (zero? n) '()
      (let (($ (pull $)))
        (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify v s/c)
  (let ((v (walk* v (car s/c))))
    (walk* v (reify-s v '()))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else  v))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v)
      (let  ((n (reify-name (length s))))
        (cons `(,v . ,n) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(define (fresh/nf n f)
  (letrec
      ((app-f/v*
        (lambda (n v*)
          (cond
           ((zero? n) (apply f (reverse v*)))
           (else (call/fresh
                  (lambda (x)
                    (app-f/v* (- n 1) (cons x v*)))))))))
    (app-f/v* n '())))

;;;

;; (define (appendo l s out)
;;   (conde
;;    ((== '() l) (== s out))
;;    ((fresh (a d res)
;;            (== `(,a . ,d) l)
;;            (== `(,a . ,res) out)
;;            (appendo d s res)))))

(define (run goal . vars)
  (take-all
   (call/goal (apply goal vars))))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(define-public (reify-all goal . names)
  (let* ((vars (map var names))
         (s/c (apply run (cons goal vars))))
    (map (lambda (s) (map (cut reify <> s) vars)) s/c)))

;; (pk (reify-all (lambda (a? b?)
;;                  (conj+ (== a? 1)
;;                         (== b? 2)))
;;                'a? 'b?))

;;;
;;; Tests
;;;

;; (define (printf message . rest)
;;   (apply format (append (list #false message) rest)))

;; (define (errorf flag message . rest)
;;   (apply printf (cons message rest)))

;; (define-syntax test-check
;;   (syntax-rules ()
;;     ((_ title tested-expression expected-result)
;;      (begin
;;        (printf "Testing ~s\n" title)
;;        (let* ((expected expected-result)
;;               (produced tested-expression))
;;          (or (equal? expected produced)
;;              (errorf 'test-check
;;                      "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
;;                      'tested-expression expected produced)))))))

;; (test-check 'run*
;;   (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
;;   '((() (1 2 3 4 5))
;;     ((1) (2 3 4 5))
;;     ((1 2) (3 4 5))
;;     ((1 2 3) (4 5))
;;     ((1 2 3 4) (5))
;;     ((1 2 3 4 5) ())))

;; (test-check 'run*2
;;   (run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))
;;   '((() (1 2 3 4 5))
;;     ((1) (2 3 4 5))
;;     ((1 2) (3 4 5))
;;     ((1 2 3) (4 5))
;;     ((1 2 3 4) (5))
;;     ((1 2 3 4 5) ())))

;; (test-check 'rember*o
;;   (letrec
;;       ((rember*o (lambda (tr o)
;;                    (conde
;;                      ((== '() tr) (== '() o))
;;                      ((fresh (a d)
;;                         (== `(,a . ,d) tr)
;;                         (conde
;;                           ((fresh (aa da)
;;                              (== `(,aa . ,da) a)
;;                              (fresh (a^ d^)
;;                                (rember*o a a^)
;;                                (rember*o d d^)
;;                                (== `(,a^ . ,d^) o))))
;;                           ((== a 8) (rember*o d o))
;;                           ((fresh (d^)
;;                              (rember*o d d^)
;;                              (== `(,a . ,d^) o))))))))))
;;        (run 8 (q) (rember*o q '(1 2 8 3 4 5))))
;;     '((1 2 8 3 4 5)
;;       (1 2 8 3 4 5 8)
;;       (1 2 8 3 4 8 5)
;;       (1 2 8 3 8 4 5)
;;       (1 2 8 8 3 4 5)
;;       (1 2 8 8 3 4 5)
;;       (1 8 2 8 3 4 5)
;;       (8 1 2 8 3 4 5)))

;; (test-check 'rember*o
;;   (letrec
;;       ((rember*o (lambda (tr o)
;;                    (conde
;;                      ((== '() tr) (== '() o))
;;                      ((fresh (a d)
;;                         (== `(,a . ,d) tr)
;;                         (conde
;;                           ((fresh (aa da)
;;                              (== `(,aa . ,da) a)
;;                              (fresh (a^ d^)
;;                                (== `(,a^ . ,d^) o)
;;                                (rember*o d d^)
;;                                (rember*o a a^))))
;;                           ((== a 8) (rember*o d o))
;;                           ((fresh (d^)
;;                              (== `(,a . ,d^) o)
;;                              (rember*o d d^))))))))))
;;        (run 9 (q) (rember*o q '(1 (2 8 3 4) 5))))
;;     '((1 (2 8 3 4) 5)
;;       (1 (2 8 3 4) 5 8)
;;       (1 (2 8 3 4) 5 8 8)
;;       (1 (2 8 3 4) 8 5)
;;       (1 8 (2 8 3 4) 5)
;;       (8 1 (2 8 3 4) 5)
;;       (1 (2 8 3 4) 5 8 8 8)
;;       (1 (2 8 3 4) 5 8 8 8 8)
;;       (1 (2 8 3 4) 5 8 8 8 8 8)))
