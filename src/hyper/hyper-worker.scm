(use-modules (uav))

(use-modules (hyper))


(define port (string->number (list-ref (program-arguments) 1)))
(define db (uav-call port))


(let loop ((height 10))
  (unless (eq? height 0)
    (format #t "* start to index urls with height ~s\n" height)
    (let next ((uids (db '(lambda (height)
                            (let ((out (query* uid? :where ((uid? 'height height)))))
                              (if (null? out)
                                  '()
                                  (map car out))))
                         height)))
      (if (null? uids)
          (begin
            (format #t "* nothing to index anymore at this height\n")
            (loop (- height 1)))
          (begin
            (catch #t
              (lambda () (index db (car uids) height))
              (lambda (key . args) (format #t "* failed with ~s\n" key)))
            (next (cdr uids)))))))
    
