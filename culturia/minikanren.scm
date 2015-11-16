;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))


(define (assp proc alist)
  "Return the first assoc from ALIST for which PROC return TRUE"
  (let next ((alist alist))
    (if (null? alist)
        #false
        (if (proc (caar alist))
            (car alist)
            (next (cdr alist))))))


(define (walk u s)
  "Find U in substitutions S"
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))


;; (acons x v substitutions)
;; ie. add (x . y) to the list of substituations
(define ext-s acons)


(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit (cons s (cdr s/c))) mzero))))


(define (unit s/c) (cons s/c mzero))
(define mzero '())


(define (unify u v s)
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
     (else (and (eqv? u v) s)))))


(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))


(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

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


;;;; How to make a simple

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))))

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

;;; Test programs

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

;; (define (appendo l s out)
;;   (conde
;;    ((== '() l) (== s out))
;;    ((fresh (a d res)
;;            (== `(,a . ,d) l)
;;            (== `(,a . ,res) out)
;;            (appendo d s res)))))


;; (define (!= u v)
;;   (lambda (s/c)
;;     (let ((s (unify u v (car s/c))))
;;       (if s mzero (unit s/c)))))


;; (define tuplespace '((1 name amirouche)
;;                      (1 age 30)
;;                      (2 name Nadoo)
;;                      (2 age 57)
;;                      (3 label friend)
;;                      (3 start 1)
;;                      (3 end 2)
;;                      (4 name Karamel)
;;                      (4 age 27)
;;                      (5 label friend)
;;                      (5 start 1)
;;                      (5 end 4)))


;; (define (attro db)
;;   (lambda (uid label value)
;;     (conde
;;      ((!= db '()) (== (list uid label value) (car db)))
;;      ((!= db '()) ((attro (cdr db)) uid label value)))))


;; (define (findo db)
;;   (lambda (attribute value out)
;;     (conde
;;      ((!= db '()) (== (cdar db) (list attribute value)) (== out (caar db)))
;;      ((!= db '()) ((findo (cdr db)) attribute value out)))))


;; (define (friendo db)
;;   (lambda (name out)
;;     (fresh (edge uid)
;;            ((findo db) 'name name uid)
;;            ((findo db) 'label 'friend edge)
;;            ((attro db) edge 'start uid)
;;            ((attro db) edge 'end out))))

;; (pk (run* (q) ((friendo tuplespace) 'amirouche q)))


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





