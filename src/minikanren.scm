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

(define-public mzero '())
(define-public (unit s/c) (cons s/c mzero))

(define-public (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit (cons s (cdr s/c))) mzero))))

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


(define-public (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define-public (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))



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

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))

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

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else  v))))

(define (reify v s/c)
  (let ((v (walk* v (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v)
      (let  ((n (reify-name (length s))))
        (cons `(,v . ,n) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

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

(define (appendo l s out)
  (conde
   ((== '() l) (== s out))
   ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res)))))


;;; my stuff

(define (disj* . gs)
  "disj+ procedure"
  (if (null? gs) (lambda (s/c) '())
      (disj (car gs) (apply disj* (cdr gs)))))

(define (conj* . gs)
  "conj+ procedure"
  (if (null? gs) (lambda (s/c) s/c)
      (conj (car gs) (apply conj* (cdr gs)))))

(define (run goal . vars)
  (take-all
   (call/goal (apply goal vars))))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(define-public (reify-all goal . names)
  "reify all variables"
  (let* ((vars (map var names))
         (s/c (apply run (cons goal vars))))
    (map (lambda (s) (map (cut reify <> s) vars)) s/c)))

(define-syntax-rule (reify* (names ...) goal)
  "sugar syntax for reify-all"
  (reify-all (lambda (names ...) goal)
             'names ...))

;;; Parsing stuff

;; (define (make-leaf! name values)
;;   (lambda (in diff^ out^)
;;     (lambda (s/c)
;;       (let ((in (walk* in (car s/c))))
;;         ((apply disj* (map (lambda (value)
;;                              (conj+ (== (car in) value)
;;                                     (== diff^ (cdr in))
;;                                     (== out^ (cons name (car in)))))
;;                              values)) s/c)))))
    

;; (define name! (make-leaf! 'NAME '(amz3 rain1 nekk)))

;; (define determiner! (make-leaf! 'DETERMINER '(a the)))

;; (define noun! (make-leaf! 'NOUN '(space
;;                                   cat
;;                                   travel
;;                                   guitar
;;                                   music
;;                                   movie
;;                                   piano)))

;; (define verb! (make-leaf! 'VERB '(create see listen play)))

;; (define question! (make-leaf! 'Q '(who how what where)))

;; ;; parse rules

;; (define (np! np diff^ out^)
;;   (conde ((fresh (out2)
;;                  (name! np diff^ out2)
;;                  (== out^ (cons 'NP out2))))
;;          ((fresh (fresh diff2 out2 out3)
;;                  (determiner! np diff2 out2)
;;                  (noun! diff2 diff^ out3)
;;                  (== out^ (cons 'NP (cons out2 out3)))))))

;; (define (s! s diff^ out^)
;;   (disj+
;;    (fresh (out2 diff2 out3 diff3 out4)
;;           (np! s diff2 out2)
;;           (verb! diff2 diff3 out3)
;;           (np! diff3 diff^ out4)
;;           (== out^ (list 'S out2 out3 out4)))
;;    (fresh (out2 diff2 out3 diff3 out4)
;;           (question! s diff2 out2)
;;           (verb! diff2 diff3 out3)
;;           (np! diff3 diff^ out4)
;;           (== out^ (list 'S out2 out3 out4)))))


;; (define (parse-sentence sentence)
;;   (caar (reify* (out)
;;                 (fresh (diff)
;;                        (s! sentence diff out)))))

;; (define the-cat-play-the-guiar (pk (parse-sentence '(the cat play the guitar))))
;; (define who-play-the-guitar (pk (parse-sentence '(who play the guitar))))
;; (pk (parse-sentence '(amz3 play the piano)))

;; (let ((sentences '((the cat play the guitar) (amz3 play the piano))))
;;   (let ((sentences (map parse-sentence sentences)))
;;     (map (lambda (s)
;;            (map pk (reify* (out)
;;                            (== (list 'S out '(VERB . play) '(NP (DETERMINER . the) NOUN . guitar)) s))))
;;          sentences)))

;; ;;; ((() (S (NP (DETERMINER . a) NOUN . cat) (VERB . play) (NP (DETERMINER . the) NOUN . guitar))))

;; (define tree '(what the fuck))

;; ;; (define (wheno matcher rewrite)
;; ;;   (

