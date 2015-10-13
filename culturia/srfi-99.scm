(define-module (srfi-99))

(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer! and set-field

;;; FIXME: this is not really srfi-99

;;;
;;; srfi-99
;;;
;;
;; macro to quickly define immutable records
;;
;;
;; Usage:
;;
;;   (define-record-type <car> seats wheels)
;;   (define smart (make-abc 2 4))
;;   (car-seats smart) ;; => 2
;;
;; Mutation is not done in place, via set-field or set-fields eg.:
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

(export define-record-type*)
(re-export set-record-type-printer!)
(re-export set-field)

