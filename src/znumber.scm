(define-module (znumber))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-60))


;;; z order curve also known as morton code
;;
;; 64bit max positive integer packing and unpacking
;;

;; zpack


(define (->u64-list v)
  (if (eq? (length v) 64)
      v
      (->u64-list (cons #false v))))


(define integer->list*
  (compose ->u64-list integer->list))


(define (zbits vs)
  (concatenate (apply zip (map integer->list* vs))))


(define (zbits->bv zbits)
  (let loop ((zbits zbits)
             (out (list)))
    (if (null? zbits)
        (list->u8vector out)
        (loop (drop zbits 8)
              (append out (list (list->integer (take zbits 8))))))))
        

(define-public (zpack vs)
  (zbits->bv (zbits vs)))


(define-public (zpack* vs)
  (zpack (map (lambda (v) (if (string? v) (string-hash v) v)) vs)))


;; zunpack


(define (->u8-list v)
  (if (eq? (length v) 8)
      v
      (->u8-list (cons #false v))))


(define (bv->zbits bv)
  (append-map integer->list bv))


(define (unzip* input count)
  (let loop ((input input)
             (out (map (lambda (x) (list)) (iota count))))
    (if (null? input)
        out
        (loop (drop input count)
              (map (lambda (x y) (append x (list y))) out (take input count))))))


(define-public (zunpack bv count)
  (let* ((->bits (compose ->u8-list integer->list))
         (zbits (append-map ->bits (u8vector->list bv))))
    (map list->integer (unzip* zbits count))))
