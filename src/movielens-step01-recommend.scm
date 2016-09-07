(use-modules (ukv) (grf3) (wiredtigerz) (wiredtiger) (ice-9 receive) (srfi srfi-26))


(define env (env-open* "/tmp/wt" (list *ukv*)))


(define (users-who-scored-high movie/id)
  (with-context env
   (let ((query (compose
		  ;; fetch the start vertex ie. users
		  (cut traversi-map start <>)
		  ;; backtrack to edge uids
		  (cut traversi-backtrack <>)
		  ;; keep values that are equal? to 5.0
		  (cut traversi-filter (lambda (x) (equal? 5.0 x)) <>)
		  ;; fetch the 'rating/value of each edge
		  (cut traversi-map (key 'rating/value) <>)
		  ;; keep edges which have 'ratin as 'label
		  (cut traversi-filter (key? 'label 'rating) <>)
		  ;; scatter...
		  (cut traversi-scatter <>)
		  ;; fetch all incomings edges 
		  (cut traversi-map incomings <>))))
      (traversi->list (query (from 'movie/id 1))))))


(define users (users-who-scored-high 1))

(define (users-top-likes users)
  (with-context env
    (let ((query (compose
                  (cut traversi-group-count <>)
                  (cut traversi-map (key 'movie/title) <>)
                  (cut traversi-map end <>)
                  (cut traversi-filter (key? 'rating/value 5.0)  <>)
                  (cut traversi-filter (key? 'label 'rating) <>)
                  (cut traversi-scatter <>)
                  (cut traversi-map outgoings <>)
                  (cut list->traversi <>))))
      (list-head (query users) 10))))

(for-each pk (users-top-likes users))
