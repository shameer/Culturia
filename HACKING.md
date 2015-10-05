# Reference API

## `<culturia>`

`<culturia>` is a hypergraph database made of `<atom>`. `<atom>` have a type,
a name, a association, an outgoing set and incomings set of `<atom>`.

- `(open-culturia path)`
- `(culturia-close culturia)`
- `(culturia-begin culturia)`
- `(culturia-commit culturia)`
- `(culturia-rollback culturia)`
- `(with-transaction culturia e ...)`

## `<atom>`

An `<atom>` is main component of culturia hypergraph database. It's defined as
an unique identifier `uid`, a scheme association `assoc` and incomings and
outgoings atoms. `<atom>` are persistent at the scheme level ie. procedure that
mutates the atom returns new records. That said, mutation is directly propagated
to the database.

- `(culturia-ref culturia uid)`

- `(create-atom culturia #:optional assoc)`
- `(atom-uid atom)`
- `(atom-assoc atom)`
- `(atom-set atom key value)`
- `(atom-ref atom key)`
- `(atom-link atom other)`
- `(atom-unlink atom other)`
- `(atom-outgoings atom)`
- `(atom-incomings atom)`
- `(atom-delete atom)`
