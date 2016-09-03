## wiredtigerz

`wiredtigerz` gather extra procedures to work with
[wiredtiger](wiredtiger.html) which aims at making the main workflow
more obvious.

It implements a declarative API to create tables with their indices and open
cursor on them. It provides a few helpers for common patterns.

`wiredtigerz` module can be found in
[culturia repository](https://github.com/amirouche/Culturia/blob/master/culturia/wiredtigerz.scm).

### Table schema

A table a schema is the specification for a given wiredtiger table.

It can be described as follow:

```scheme
(table-name
 (key assoc as (column-name . column-type))
 (value assoc as (column-name . column-type))
 ((list indices as (indexed-ame (indexed keys) (projections column names)))))
```

`column-type` are verbose names for column types:

- `record`

- `string`

- `unsigned-integer` or `positive-integer`

- `integer`

- `raw`

An example of a configuration that defines an `posts` table with `uid`, `title`,
`body`, `published-at` fields and one index one `published-at` with a projectionñ
on `uid` column:

```
(define posts '(posts
 ((uid . raw))
 ((title . string) (body . string) (published-at . raw))
 ((published-at (published-at) (uid)))))
```

This is reference through the API as `config` argument or `configs`
when the procedure expects a list of schemas.

Have a look at the
[official documentation](http://source.wiredtiger.com/develop/schema.html#schema_index_projections)
to learn what projections are.

### Reference API

#### (scm->string scm)

Write a scm value to a string.

#### (string->scm string)

Read a scm value form a string.

#### <env>

##### (env-open* path configs) -> env

Open and init an environment at `PATH` using `configs` table
schemas. This is will create the tables specified by `configs` if
required and return an `<env>` record. 

`<env>` records are threadsafe.

##### (env-close env)

Close the environment.

##### (with-context env body ...)

Set for the dynamic state a `<context>` as current *context*. You can
then use `call-with-cursor` to use a cursor from that context. You can also
use `context-begin`, `context-commit` and `context-rollback` or most
likely the sugar syntax `with-transaction` to manipulate the
transaction state.

#### (call-with-cursor name proc)

Calls `PROC` with the cursor named `NAME` inside the current context.

Must be called inside a `with-context` or `with-env`.

`PROC` *can not* be lazy, the operations on the cursor must be
finished when it returns and it must reset the cursor if needed.

##### (with-env env body ...)

Sugar syntax to use an environment, set the current context and close
the environment it when finished.

##### (with-transaction body ...)

Execute `BODY ...` inside a transaction, can throw a `wiredtiger`
exception.  If `BODY ...` throw an exception. It's catched, the
transaction is rollbacked and the exception is re-throw.

#### Cursor navigation

##### (cursor-next* cursor)

Calls `cursor-next` and return `#false` if `wiredtiger` exception is
thrown. This usually (all the time?) means that there is not next
record.

##### (cursor-prev* cursor)

Calls `cursor-prev` and return `#false` if `wiredtiger` exception is
thrown. This usually (all the time?) means that there is not previous
record.

##### (cursor-value-ref* cursor . key)

Retreive the value associated with key in cursor.

##### (cursor-insert* cursor key value)

Insert `VALUE` at `KEY` using `CURSOR`. If the cursor was opened in
*append* mode you must pass `#nil` or `'()` as `KEY`.

##### (cursor-update* cursor key value)

Update `KEY` with `VALUE` using `CURSOR`.

##### (cursor-remove* cursor . key)

Remove `KEY` using `CURSOR`.

##### (cursor-search* cursor . key)

Search `KEY` using `CURSOR`.

##### (cursor-search-near* cursor . key-prefix)

Prepare `CURSOR` for forward search using `KEY-PREFIX`.

##### (cursor-range cursor . key-prefix)

Return the list of **values** that match `KEY-PREFIX` using `CURSOR`.

##### (cursor-range-prefix cursor . key-prefix)

Return the list of **key/value pairs** that match `KEy-PREFIX` using `CURSOR`.

