(use-modules (ukv) (grf3) (wiredtigerz) (wiredtiger) (ice-9 receive) (srfi srfi-26))


(define env (env-open* "/tmp/wt" (list *ukv*)))


(define (users-who-scored-high movie/id)
  (with-context env
   (let ((query (compose
		  ;; fetch the start vertex ie. users
		  (cut traversi-map start <>)
		  ;; backtrack to edge uids
		  (cut traversi-backtrack <>)
		  ;; keep values that are at least 4.0
		  (cut traversi-filter (lambda (x) (<= 4.0 x)) <>)
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
                  ;; retrieve movie vertex's title
                  (cut traversi-map (key 'movie/title) <>)
                  (cut traversi-map end <>)
                  ;; retrieve rating edges with a score of at least 4.0
                  (cut traversi-backtrack <>)
                  (cut traversi-filter (lambda (x) (<= 4.0 x)) <>)
                  (cut traversi-map (key 'rating/value) <>)
                  (cut traversi-filter (key? 'label 'rating) <>)
                  (cut traversi-scatter <>)
                  (cut traversi-map outgoings <>)
                  (cut list->traversi <>))))
      (list-head (query users) 10))))

(for-each pk (users-top-likes users))

(env-close env)
