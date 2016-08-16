(define-module (hn))

(use-modules (srfi srfi-1))
(use-modules (rnrs io ports))
(use-modules (ice-9 receive))
(use-modules (ice-9 threads))
(use-modules (http))
(use-modules (msgpack))


(define (download url)
  (catch #t
    (lambda ()
      (let ((response (curl url)))
        (pack `(("url" . ,url) ("response" . ,response)))))
    (lambda _ (pk _) '())))


(define (store bv)
  (unless (null? bv)
    (display ".")
    (let ((port (open-file "hn.msgpack" "ab")))
      (put-bytevector port bv)
      (close port))))

(define urls (list "https://www.dashingd3js.com/data-visualization-and-d3-newsletter/data-visualization-and-d3-newsletter-issue-80"
                    "http://www.kalzumeus.com/2014/05/12/conversion-optimization-in-practice-baconbiz-2013-presentation/"
                    "http://weuse.meteor.com/"))

(define (dump)
  (n-for-each-par-map 1 store download urls))

(define (load)
  (let* ((filename "hn.msgpack")
         (file (open filename  O_RDONLY)))
    (let next-entry ((entry (get-unpack file)))
      (unless (eof-object? entry)
        (pk entry)
        (next-entry (get-unpack file))))))

(dump)
(load)
        
