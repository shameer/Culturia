# culturedb


## Kesako culturedb?

`culturedb` is hypergraph database ie. a graph database with single
kind of entity the `<atom>` which is linked to other `<atom>`. It's
similar to a graph database, but the datamodel simpler *ahem* more
generic.

It also comes with two kind of indices: exact and fuzzy.

## Reference API

Procedures with a exclamation mark suffix `!` mutate the database.

Throught this documentation `CONTEXT` is a wiredtiger**z** database
context created with `context-open`.

### `<atom>` API

#### `(atom-uid atom)

Return the unique identifier of `ATOM`.

#### `(atom-assoc atom)`

Return the assoc of `ATOM`.

#### `(create-atom atom #:optional (assoc '()))`

Create an `<atom>` with `ASSOC`.

#### `(atom-set atom key value)`

Shortcut to set `ATOM`'s assoc this returns a new `<atom>` record.

#### `(atom-ref atom key)`

Shortcut to reference `ATOM` assoc.

#### `(atom-insert! atom context)`

Insert as new atom `ATOM` in the wiredtiger database referenced by `CONTEXT`.

#### `(atom-update! atom context)`

Update `ATOM` inside the database reference by `CONTEXT`.

#### `(atom-ref* uid context)`

Reference `<atom>` with `UID` as unique identifier inside the database
referenced by `CONTEXT`.

#### `(atom-link! atom other context)`

Create a directect link between `ATOM` and `OTHER` inside the database
referenced by `CONTEXT`

#### `(atom-incomings atom context)`

Retrieve every **incomings** links of `ATOM` found inside the database
referenced by `CONTEXT`.

#### `(atom-outgoings atom context)`

Retrieve every **outgoings** links of `ATOM` found inside the database
referenced by `CONTEXT`.

#### `(atom-unlink! atom other context)`

Remove the directed link between `ATOM` and `OTHER` inside the database
referenced by `CONTEXT`.

#### `(atom-remove! atom context)`

Remove `ATOM` from the database referenced by `CONTEXT`.

### index API

#### `(index-set! key value context)`

Create an exact index as `KEY` on any scheme `VALUE` in database
referenced by `CONTEXT`.

#### `(index-ref key context)`

Reference scheme value indexed at `KEY` in the database referenced by
`CONTEXT`

### fuzzy index

#### `(fuzzy-index! word value context)`

Fuzzy index scheme `VALUE` as `WORD` inside the database referenced by
`CONTEXT`.

#### `(fuzzy-search word context)`

Fuzzy search `WORD` inside the database referenced by `CONTEXT`.

Returns top 10 results according to levenstein distance.
