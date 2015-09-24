(use-modules (culturia))

;; create the database
(define nl2 (create-culturia "/opt/nl2")) ;; aka. the very first robot to trick humans

;; checkout the master branch, which the default branch in newly created database
;; this returns an <version>, is the handle over the current revision of the database
(define master (checkout-version nl2 "master"))

;; the database was just created, it has only one culture
(define main (version-culture-ref master "main"))

;; let's create a few <atom> the most basic and most important element in culturia database
;; to represent the following four rules
;;
;; If X croaks and X eats flies - Then X is a frog
;; If X chirps and X sings - Then X is a canary
;; If X is a frog - Then X is green
;; If X is a canary - Then X is yellow


;; let's add a few nouns
(define fritz (make-atom main "noun" "fritz"))
(define fly (make-atom main "noun" "fly"))
(define frog (make-atom main "noun" "frog"))
(define canary (make-atom main "noun" "canary"))
(define green (make-atom main "noun" "green"))
(define yellow (make-atom main "noun" "yellow"))

;; add a few verbs
(define croak (make-atom main "verb" "croak"))
(define chirps (make-atom main "verb" "chirps"))
(define eat (make-atom main "verb" "eat"))
(define sing (make-atom main "verb" "sing"))

;; ;; create `if ... then ...` logic relation and the logical operator `and`
;; (define ifthen (make-atom main "relation" "if then"))
;; (define and (make-atom main "relation" "and"))
;; (define is (make-atom main "verb" "is"))

;; To link an atom to another use `atom-link`

(atom-link is fritz) ;; fritz is
(atom-link is frog) ;; is frog

;; Links are ordered by the order of insertion
;; an <atom> has both incoming and outgoing set.
;; So, `is` has both  `fritz`  and `frog` in its outgoing set
;; in this order
(assert (equal? (atom-outgoings is) (list fritz frog)))

;; we can retrieve all atoms of the main culture using
(culture-atoms main)


        
