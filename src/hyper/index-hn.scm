(use-modules (hyper) (graphitisay) (wiredtiger) (wiredtigerz))
(use-modules (ice-9 receive) (srfi srfi-26))
(use-modules (system vm trace))
(use-modules (web response))
(use-modules (https-get))
(use-modules (ice-9 iconv))
(use-modules (json))


(setlocale LC_ALL "")


(define (maxitem)
  (receive (response body) (https-get "https://hacker-news.firebaseio.com/v0/maxitem.json")
    (string->number (bytevector->string body "utf-8"))))

(receive (cnx ctx) (apply wiredtiger-open* (cons "/tmp/wt/" *hyper*))
  (let loop ((uid (maxitem)))
    (let ((url (format #f "https://hacker-news.firebaseio.com/v0/item/~a.json" uid)))
      (receive (response body) (https-get url)
        ;; convert json to scheme
        (let ((doc (cdr (call-with-input-string (bytevector->string body "utf-8") read-json))))
          ;; check that this is story with a least a score of 2
          (when (and (equal? (assoc-ref doc "type") "story")
                     (assoc-ref doc "url")
                     (< 2 (assoc-ref doc "score")))
            (url-index!* ctx (assoc-ref doc "url")))
          (loop (1- uid)))))))
  
