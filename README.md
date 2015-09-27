# Culturia

Culturia is clone of [OpenCog](http://opencog.org/) 's database [AtomSpace]() written in [Guile](https://www.gnu.org/software/guile/) and using [wiredtiger](http://wiredtiger.com/).

This software is free software work licensed under the AfferoGPLv3.

The name is a reference to [Culture and Empire by Pieter Hintjens](http://cultureandempire.com).

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

### `<atom>`

#### `(culturia-atom-ref culture type #:optional name)`

#### `(culturia-atom-ref/uid culture uid)`

#### `(culturia-create-atom culture type name data)`

#### `(atom-uid atom)`

#### `(atom-type atom)`

#### `(atom-name atom)`

#### `(atom-outgoings atom)`

#### `(atom-incomings atom)`

#### `(atom-data-ref atom)`

#### `(atom-data-set atom data)`
