(use-modules (ice-9 rdelim))
(use-modules (ice-9 receive))
(use-modules (sxml xpath))

(use-modules (path))
(use-modules (text))

(use-modules (htmlprag))

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (grf3))
(use-modules (ukv))
(use-modules (wsh))


(define (remove-newlines string)
  (string-map (lambda (char) (if (equal? char #\newline) #\space char)) string))

(define (make-snippet string)
  (if (< 200 (string-length string))
      (remove-newlines (string-take string 200))
      (remove-newlines string)))

(define (add-document url title snippet)
  (receive (new vertex) (get-or-create-vertex 'url/url url)
    (let* ((vertex (vertex-set vertex 'url/title title))
           (vertex (vertex-set vertex 'url/snippet snippet)))
      (save vertex))))

(define (document-ref url)
  (let ((document (traversi->list (from 'url/url url))))
    (if (null? document)
        #false
        (get (car document)))))

(define (index* path)
  (let* ((name (basename path))
         (url (string-append "https://en.wikipedia.org/wiki/" name)))
    (pk name)
    (with-input-from-file path
      (lambda ()
        (let ((response (read-string)))
          ;; parse html
          (let ((html (html->sxml response)))
            ;; try to read title or use NAME
            (let ((title (let ((title ((sxpath '(// head title *text*)) html)))
                           (if (null? title)
                               name
                               (car title)))))
              ;; make snippet from body
              ;; XXX: doing html2text over response includes the title of
              ;; the page with extra stars chars which is ugly, here we
              ;; fetch only the body and create a snippet from that this
              ;; aint perfect because headings are also converted to text
              ;; using star chars
              (let ((body ((sxpath '(// body)) html)))
                (unless (null? body)
                  (let* ((body (with-output-to-string (lambda () (sxml->html body))))
                         (body (html2text body))
                         (snippet (make-snippet body)))
                    ;; add document to the database
                    (add-document url title snippet)
                    ;; index it
                    (index url response)))))))))))
  
(with-env (env-open* "/tmp/wt" (cons* *ukv* *wsh*))
  (path-walk "data/wikipedia.en/A" index*))
