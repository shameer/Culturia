title: Reference API: Guile Wiredtigerz
data: 2015-10-22 23:59


## wiredtigerz

`wiredtigerz` gather extra procedures to work with `guile wiredtiger` which aims
at making the main workflow more obvious.

It implements a declarative API to create tables with their indices and open
cursor on them. It provides a few helpers for common patterns.

`wiredtigerz` module can be found in [culturia repository](https://github.com/amirouche/Culturia/blob/master/culturia/wiredtigerz.scm).

## Reference API

### (session-create* session . configs)

`session-create*` will create the tables and indices defined declarativly in
`configs`. a `config` must looks like the following:

``` ascii
(table-name
 (key assoc as (column-name . column-type))
 (value assoc as (column-name . column-type))
 ((list indices as (indexed-ame (indexed keys) (projections column names)))))
```

`column-type` are verbose names for column types:

- `record`

- `string`

- `unsigned-integer`

- `integer`

- `raw`

An example of a configuration that defines an `posts` table with `uid`, `title`,
`body`, `published-at` fields and one index one `published-at` with a project
on `uid` column:

```scheme
(define posts '(posts
 ((uid . raw))
 ((title . string) (body . string) (published-at . raw))
 ((published-at (published-at) (uid)))))
```

You can create the table and indices in one call using the following code:

```scheme
(define connection (connection-open "/tmp/wiredtigerz" "create"))
(define session (session-open connection))
(session-create* session posts)
(session-close session)
```

### (cursor-open* session . configs)

`(cursor-open* session config)` will open all the cursors related to a given
`config` as an assoc.

You can open cursors over the table and indices you created using
`session-open*`. It use the same syntax and the same table declaration.

```scheme
(define connection (connection-open "/tmp/wiredtigerz" "create"))
(define session (session-open connection))
(define cursors (cursor-open* session posts))
```

`cursors` is an assoc that maps table name as symbol to its cursor and indices
to their cursor. An extra *append* cursor will be created if the table has a
single raw column. Index and append cursors are prefixed by the table name.
Which means that the above `cursors` should contain the following keys:

```scheme
(list 'posts 'posts-append 'posts-published)
```

Mind the fact that keys are symbols. Also `posts-published` cursor has `uid` as
cursor's value since a projection was done. Leave the field empty for the
default behavior. If there is no indices, leave the indices list empty.

### Simple database

#### (wiredtiger-open path . configs)

Open database at `path`, create tables using `configs` if necessary and return
a pair `(connection . session)` and a `cursors` assoc as returned
by `cursor-open*`.

This procedure is useful in a context where you don't plan to use threads.


#### (wiredtiger-close database)

Shortcut procedure to close a database where `database` is a pair of connection
and session.


### Context

Context is made of `<session>` and `cursors` assoc. This is useful in multithread
settings if you don't need to open multiple cursors for the same table.

### (context-open connection . configs)

`cursor-open*` sister procedure that will open a session and `cursors` assoc
and return a context.

### (context-session context)

Return the session associated with `context`

### (context-ref context name)

Return the cursor `name` from `context`.

### transactions

Use `(context-begin context)`, `(context-commit context)` and
`(context-rollback context)` to work with transactions.

There is macro helper `(with-transaction context e ...)` that begin and
commit a transaction at end.

### Cursor navigation

#### (cursor-value-ref* cursor . key)

Retreive the value associated with key in cursor.

#### (cursor-insert* cursor key value)

Insert `value` at `key` using cursor. If `key` is `#nil`, insert the `value`
directly.

#### (cursor-update* cursor key value)

Update `key` with `value` using `cursor`.

#### (cursor-remove* cursor . key)

Remove `key` using `cursor`.

#### (cursor-search* cursor . key)

Search `key` using `cursor`.

#### (cursor-search-near* cursor . key-prefix)

Prepare `cursor` for forward search using `key-prefix`.

#### (cursor-range cursor . key-prefix)

Return the list of key/value pairs that match `key-prefix` using `cursor`.
