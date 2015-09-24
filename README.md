# Culturia

Culturia is a prototype backing storage for [OpenCog](http://opencog.org/)
's database [AtomSpace]() written in [Guile](https://www.gnu.org/software/guile/)
and using [wiredtiger](http://wiredtiger.com/).

This software is free software work licensed under the AfferoGPLv3.

The name is a reference to [Culture and Empire by Pieter Hintjens](http://cultureandempire.com).

## Goals

Prototype a backend storage for [AtomSpace](http://wiki.opencog.org/w/AtomSpace)
using wiredtiger and Guile.

- Handle bigger than RAM databases on a single node 
- Implement the current AtomSpace features 
- Implement Hierarchical AtomSpaces cf. Multiple AtomSpaces 
- Implement spacio-temporal indices

And implement if there is interest a version control system (VCS).

### Sub goals

- Prepare a design that can be transposed to C/C++
- Prepare a design that can be transposed to a distributed key/value store 

### Stretch goals

- Fully API compatible with OpenCog
- Create a mini-opencog that is easy to study and hack
- Solve NLP tasks using opencog principles to get a better understand of opencog
- Create a feedback loop pipeline on top of wikibase and other wikis
- Explore program generation 

### Side goals

Things that i'd like to do:

- Fully implement Tinkerpop Gremlin query DSL
- Explore interaction between pattern matching and Gremlin
- Implement Earley parser to do pattern matching
- Make it easier to work with, like allow mutability and add properties to atoms. 

## Reference API

### `<culturia>`

#### `(create-culturia path)`

Create a `<culturia>` at `PATH`.

#### `(open-culturia path)`

Open `<culturia>` found at `PATH`.

#### `(culturia-close culturia)`

Close `<culturia>`.

#### Transactions

- `(culturia-begin culturia)`
- `(culturia-commit culturia)`
- `(culturia-rollback culturia)`
- `(with-transaction culturia e ...)`

### `<revision>`

A `<culturia>` database is versioned in similar way as a filesystem can be
versioned using a git or mercurial. Instead of having directories and files
there is `<culture>` and `<atom>`.

#### `(checkout-revision culturia name)`

Retrieve `NAME` branch in of `CULTURIA`.

#### `(revision-commit revision commit)`

Commit modifications done in the database and return the new revision.

### `<culture>`

#### `(create-culture revision name #:optional parent)`

#### `(checkout-culture revision name)`

#### `(culture-atoms culture)`

### `<atom>`

#### `(checkout-atom culture type #:optional name)`

#### `(checkout-atom/uid culture uid)`

#### `(create-atom culture type name data)`

#### `(atom-uid atom)`

#### `(atom-type atom)`

#### `(atom-name atom)`

#### `(atom-outgoings atom)`

#### `(atom-incomings atom)`

#### `(atom-data-ref atom)`

#### `(atom-data-set atom data)`
