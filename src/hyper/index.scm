(use-modules (hyper) (graphitisay) (wiredtiger) (wiredtigerz))

(use-modules (ice-9 receive) (srfi srfi-26))
(use-modules (system vm trace))

(receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt/" *hyper*))
  (domain-index! ctx "http://www.hypermove.net")
  (domain-index! ctx "http://www.hyperdev.fr")
  ;; (domain-index! ctx "http://dustycloud.org/")
  ;; (domain-index! ctx "http://www.gnu.org")

  (pk 'count
      (let ((cursor (cursor-open (context-session ctx) "table:vertex")))
        (cursor-reset cursor)
        (let loop ((count 0))
          (if (catch 'wiredtiger
                (lambda () (cursor-next cursor) #t)
                (lambda _ #f))
              (loop (1+ count))
              count))))

  ;; (pk (vertex-outgoings ctx (token-get-or-create ctx "hypermove")))
  (for-each (lambda (k) (pk (cons (url-ref ctx (car k)) (cdr k)))) (search** ctx "hypermove"))

  (connection-close cnx))
