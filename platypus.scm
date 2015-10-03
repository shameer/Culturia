Code is better than a hundred words. I will try to translate a AtomSpace query/pattern to Gremlin to show how one can related to the other.

Gremlin is lower level.

Given the following PLN query, that defines everything that breathe. It use use (PredicateLink "breathe") to define things that breathe air with a confidence ie. ThruthValue of 0.8:

(PredicateLink "breathe"
               (InheritanceLink
                (VariableNode "$entity")
                (ConceptNode "air")
                ThruthValue ".8"))

This is a bit different from the PredicateLink propasal since it takes a NumericValue but it's not specified what kind of NumericValue it takes.

Given the following database:

;; define atoms
(define inheritance-link (make-atom "inheritance-link" #:name "platypus breathe air" #:thruth-value 0.8))
(define platypus (make-atom "concept" #:name "platypus"))
(define air (make-atom "concept" #:name "air"))
(define breathe (make-atom "concept" #:name "breathe"))

;; link everything together
(atom-link platypus inheritance-link)
  
(atom-link inheritance-link breathe)
(atom-link inheritance-link air)

We want can *check* that a platypus match the query using the following gremlin query:

(define search-things-that-breath (gremlin incomings
                                          (type "inheritance-link")
                                          ;; it must have an outgoing arrow to "breathe" concept
                                          (filter (gremlin outgoings (field "name") (eq? "breathe")))
                                          ;; it must have have least 0.8 thruth value
                                          (filter (gremlin (field "thruth-value") (>= 0.8)))
                                          incomings))

(search-things-that-breath air) ;; returns (list platypus)

The *link* chase is done with the filters.

This can be inefficient, in the case where <air> has more incoming atom than the total number
of atoms in the database matching the filters. This is similar what happens when one swap
inner join with outer join. To avoid this, we can use the a z-index that is built like this:

(make-index database indexer)

where the indexer is a procedure that takes an atom as first argument and return an set of
named values.

(define (indexer atom)
  (list
   (cons 'truth-value (atom-ref atom 'truth-value))
   (cons 'name (atom-ref atom 'name))))

The set of named values are inserted in the index defined as:

(define name-and-truth-value (make-zindex 'truth-value 'name))

Because that's the keys returned by the indexer procedure. Under the hood it does convert
string values to numerical values.

Anyway, we can then retrieve *directly* things that breath with a confidence of at least 0.8:

(define things-that-breath (zindex name-and-truth-value (>= 0.8) "breathe"))

And use it as seed for a minimal gremlin traverser that retrieves things that breathe air with a confidence of at least 0.8:

((gremlin incomings) things-that-breath) ;; returns (list platypus)

To complete the above, it must be noted that *could* be possible to also use the zindex
inside the first gremlin query with something like:

(define search-things-that-breath (gremlin incomings
                                           (type "inheritance-link")
                                           (zindex-filter name-and-truth-value (>= 0.8) "breathe")
                                           incomings))

where the zindex-filter foo will check for the existence of the atom currently in the traversal
in the zindex. It's not useful in this case because zindex can serve as a seed. But depending on the transpilation strategy between PLN and gremlin the last approach is more general.

The above try to explain show how gremlin works and how it can be used as a lower level
api to query the graphdb.

