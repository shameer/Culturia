# Culturia

Culturia is clone of [OpenCog](http://opencog.org/)'s database [AtomSpace]()
written in [Guile](https://www.gnu.org/software/guile/) and using
[wiredtiger](http://wiredtiger.com/).

This software is free software work licensed under the AfferoGPLv3.

The name is a reference to [Culture and Empire by Pieter Hintjens](http://cultureandempire.com).

## Reference API

### `<culturia>`

#### `(culturia-create path)`

Create a `<culturia>` at `PATH`.

#### `(culturia-open path)`

Open `<culturia>` found at `PATH`.

#### `(culturia-close culturia)`

Close `<culturia>`.

#### `(culturia-begin culturia)`

#### `(culturia-commit culturia)`

#### `(culturia-rollback culturia)`

#### `(with-transaction culturia e ...)`

### `<atom>`

#### `(culturia-atom-create culturia type name)`

#### `(culturia-atom-ref/uid culturia uid)`

#### `(culturia-atom-ref culturia type #:optional name)`

#### `(atom-uid atom)`

#### `(atom-type atom)`

#### `(atom-name atom)`

#### `(atom-assoc atom)`

#### `(atom-assoc-set atom key value)`

#### `(atom-assoc-ref atom key)`

#### `(atom-link atom other)`

#### `(atom-outgoings atom)`

#### `(atom-incomings atom)`

