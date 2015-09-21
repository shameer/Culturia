# (use-modules (tpldb))

`tpldb` is a simple tuple based server-less database.

## Getting started

At the very core this database is built around the concept of tuples
ie. fixed size list. The database will keep them ordered using the
natural order. For instance the following might appear in the database:

```scheme
("ABCDEFGH" "post/annotations" ("tpldb" "wiredtiger"))
("ABCDEFGH" "post/body" "This is how you use tpldb you start with a set of ordered tuples")
("ABCDEFGH" "post/title" "Getting started with tpldb")
("IJKLMNOP" "post/annotations" ("tpldb" "wiredtiger"))
```
So the tuple with `IJKLMNOP` appears last because the identifier comes
after `ABCDEFGH`.

This tuples are `ìav` tuples which stands for:

- identifier (aka. unique identifier string)
- attribute (aka. attribute name string)
- value (any scheme value)

`i` can be generated if you don't have one via `tpldb-make-uid`. `tpldb-ref`
which allows to fetch all tuples associated with a `uid` can be 

This is the primary way to access the database. There is procedure called
`tpldb-iav-map` that allows to manipulate the database following the `ìav`
ordering.

The above ordering might be inneficient in some situation. Thanks to wiredtiger
another index is built called `avi` which stands for
`(attribute value identifier)` again this tuple space (or table) is ordered.

The previous set of tuple will be ordered in this tuple space like that:

```scheme
("post/annotations" ("tpldb" "wiredtiger") "ABCDEFGH" )
("post/annotations" ("tpldb" "wiredtiger") "IJKLMNOP")
("post/body" "This is how you use tpldb you start with a set of ordered tuples" "ABCDEFGH")
("post/title" "Getting started with tpldb" "ABCDEFGH")
```
The order of the first tuple is not guaranteed but it is certain that tuples
having `post/annotations` attribute will come first. This tuple space allows
via `tpldb-avi-map` to:

- iterate over all objects having a given `attribute`
- iterate over all object having a given `attribute` and `value`.

## Reference API

### `(create-tpldb path)`

Create a database at `path`.

### `(tpldb-close tpldb)`

Close the database. The database must be close to guarantee that everything
is persisted correctly.

### `(tpldb-begin db)`

Start a transaction.

### `(tpldb-rollback`

Rollback a transaction.

### `tpldb-commit`

Commit a transaction.

### `tpldb-make-uid`

Create a unique identifier. This must be run in a transaction with the
set of tuples that must be associated with this identifier. See `tpldb-add`.

### `(tpldb-add tpldb uid attribute value)`

Add the tuple `(uid attribute value)` to the database. Attribute must be a
string and value any base scheme data type. The database will sort tuples
by natural order. For `value` column, integers comes first, then string
and eventually scheme values which have no precise order.

### `(tpldb-ref tpldb uid)`

Retrieve all tuples associated with uid.

### `(tpldb-del tpldb uid)`

Removes all tuples associated with `uid`.

### `(tpldb-iav-map tpldb proc uid [attribute ""])`

Iterate over all tuples associated with `uid`. If `attribute` can be specified
it will iterate over all tuples with prefix `(uid attribute)`. `proc` receives
three arguments `uid`, `attribute` and `value`.

### `(tpldb-avi-map tpldb proc attribute value)`

Iterate over all tuples associated with `attribute`. If `value` can be specified
it will iterate over all tuples with prefix `(attribute value)`. `proc` receives
three arguments `attribute`, `value` and `uid` in this order.
