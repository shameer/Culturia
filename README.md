# Culturia

Culturia is clone of [OpenCog](http://opencog.org/)'s database [AtomSpace]()
written in [Guile](https://www.gnu.org/software/guile/) and using
[wiredtiger](http://wiredtiger.com/).

This software is free software work licensed under the AfferoGPLv3.

The name is a reference to [Culture and Empire by Pieter Hintjens](http://cultureandempire.com).

## 2015/09/22 - What Are The Civilian Applications?

I started the the implementation and pushed some code. This includes FFI
bindings of wiredtiger with documentation and a few examples (that may not work
as-is). The best way to get started with wiredtiger is to read
[Schema, Columns, Column Groups, Indices and Projections](http://source.wiredtiger.com/2.6.1/schema.html).

## 2015/09/27 - Well I Was In The Neighbourhood

I tried several designs that includes:

- **version control system**: this works using copy-on-write and filtering stored atoms by *revision path*. Things that must be taken into account:
  - moving from one "workspace" to another. This requires to remove all changes that have been done in the "workspace". Other bookeeping scheme like transaction/multiple state revision so that it's possible for several thread to work on different *workspace*. 
  - and more interestingly provide an API that allows to browser the history of a single node and continue graph exploration from there.
  
- **hierarchical hypergraph**: Simple hypergraph inheritance can use a similar model
  to version control system. You build a tree (or forest) using a set of tuples
  then that you cache in memory. Then when fetching atoms, you check that those
  atoms are part of the correct *tree branch path*
  
- *multiple parents hypergraph*: extending the above allowing parent atoms to
  have multiple parents. Not sure what the use of this

I end up thinking really about recursive hypergraph where atoms could freely
link to other atoms but be part of a more general structure. Then comes the idea
of *recursive fuzzy hypergraph* which really is the thing. It allows to model
how for instance a “word” has 90% of time the sens of “an atomic semantic token of sentence” and 10% of time the sens of “a message”. Given that it ca
be implemented using a simple hypergraph efficently there is not need to go
further.

Anyway, all this need more time to mature. Another I need to tackle is the
ability to run similar algorithm fully in memory and database backed.
