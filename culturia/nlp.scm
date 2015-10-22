(define-module (nlp))

(use-modules (ice-9 match))


(define *abbreviations* (list "Ms." "M."))

(define (abbreviation? word)
  (list-index *abbreviations* word))


(define (split-sentences paragraph)
  (let next ((paragraph (string->list paragraph))
             (sentences (list (list (list)))))
    (match paragraph
      ((#\. #\space char . rest)
       (next rest (cons (list (list char)) sentences)))
      ((#\space . rest)
       (next rest (cons (cons (list) (car sentences)) (cdr sentences))))
      ((or (#\.) (#\. #\. #\.))
       (reverse (map (lambda (sentence) (reverse (map (compose list->string reverse) sentence))) sentences)))
      ((char . rest)
       (next rest (cons (cons (cons char (caar sentences)) (cdar sentences))
                              (cdr sentences)))))))



(pk 'out (split-sentences "Noa eats a kiwi. The kiwi is good."))

    
    
