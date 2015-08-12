# guile-wiredtiger 0.2

*build your own database*


- ACID
- networkless
- automatic index
- multithread support
- ordered key/value store


## Informations

- Langue: Scheme GNU Guile
- Tested with *wiredtiger 2.6.1*
- License: GPL2+ (same as wiredtiger)
- Join us at **irc.freenode.net#guile** for support.
- Author: [amirouche](mailto:amirouche@hypermove.net)


## (use-modules (wiredtiger))

Create the `/tmp/wt` directory before running the following example:

```scheme
(define connection (pk (connection-open "/tmp/wt" "create")))
(define session (pk (session-open connection)))

;; create a table
(session-create session "table:nodes" "key_format=i,value_format=S")

;; open a cursor over that table
(define cursor (pk (cursor-open session "table:nodes")))

;; start a transaction and add a record
(session-transaction-begin session "isolation=\"snapshot\"")
(cursor-key-set cursor 42)
(cursor-value-set cursor "The one true number!")
(cursor-insert cursor)
(session-transaction-commit session)

(cursor-reset cursor)
(cursor-next cursor)
(pk (cursor-key-ref cursor))
(pk (cursor-value-ref cursor))
(cursor-close cursor)
(session-close session)
(connection-close connection)
```

## Kesako wiredtiger?

At the very core, it's a configurable ordered key/value store, column aware,
with global transactions.

wiredtiger is a versatile database built by the engineers who created
Oracle Berkeley Database (formely known as Sleepycat Database and bsddb)
to be the best of its kind taking advantage of new hardwares.

It's not only a database. With wiredtiger you can build fine tuned
databases and competitive generic databases like mysql or mongodb.

It can provide a performance boost for your application but also simplify
its design by using a schema that is simpler than the one you would use
with SQLite.

## Installation

wiredtiger does **not** work on 32 bits architectures. It was tested with
wiredtiger `2.6.1` but might work with future versions.

You need to install wiredtiger (tested with 2.6.1) with the usual
`./configure && make && make install` cli dance. If you prefer to
use git:

```
git clone https://github.com/wiredtiger/wiredtiger.git
```

You also need a recent version of GNU Guile (tested with 2.0.11). It's available
in most GNU/Linux distributions.

## Reference API

A lot of the API is available, what remains can be the subject of patches :)

The following documentation doesn't cover all of wiredtiger, it is best to have
a look at [wiredtiger manual](http://source.wiredtiger.com/2.6.1/index.html)
too.

They are three objects in guile-wiredtiger:

- `<connection>` a repsent a particular connection to the wiredtiger engine.
  ACID is not supported across several instance of  `<connection>`.
- `<session>` has a `<connection>` as parent. It's not threadsafe.
- `<cursor>` has a `<session>` as parent.

The last section is about the optional but useful *packing procedures*. This is
important to read this section to better understand wiredtiger and debug
your programs.

### connection

#### `connection-open home config) -> connection`

Open a connection to a database. Most applications will open a single connection
to a database. A connection can be shared among several threads. There is no
support ACID transactions between several connections.

Example:

```
(connection-open "./databases/magic-numbers" "create,cache_size=500M")
```

`home` the path to the database home directory. The path must exists.
See [Database Home Directory for more information]().

#### `(connection-close connection [config]) -> boolean`

Close connection. Any open sessions will be closed. config optional argument,
that can be `leak_memory` to not free memory during close.


### session

#### `(session-open connection [config]) -> <session>`

Open a session.

Example:

```
(session-open connection "isolation=snapshot")
```

`config` configures isolation level:

- `read-uncommited` transactions can see changes made by other transactions
  before those transactions are committed. Dirty reads, non-repeatable reads
  and phantoms are possible.

- `read-commited` transactions cannot see changes made by other transactions
  before those transactions are committed. Dirty reads are not possible;
  non-repeatable reads and phantoms are possible. Committed changes from
  concurrent transactions become visible when no cursor is positioned in the
  read-committed transaction.

- `snapshot` transactions read the versions of records committed before the
  transaction started. Dirty reads and non-repeatable reads are not possible;
  phantoms are possible.

If you don't know what you are doing, use `snapshot`.

#### `(session-close session)`

Close the session handle. This will release the resources associated with the
session handle, including rolling back any active transactions and closing any
cursors that remain open in the session.

All data operations are performed in the context of a session. This encapsulates
the thread and transactional context of the operation.

Thread safety: A session is not usually shared between threads, see
Multithreading for more information.

#### `(session-create session name config)`

Create a table, column group, index or file.

Example:

```
(session-create session "table:magic-numbers" "key_format=i,value_format=S")
```

`name` the URI of the object to create, such as `"table:stock"`. For a
description of URI formats see **Data Sources**.

`config` configures the object.

In guile-wiredtiger, `key_format` and `key_value` only support integral types
`bBhHiIlLqQr` and variable length strings `S`. See format types for more
information.

#### `(session-transaction-begin session [config])`

Start a transaction.

#### `(session-transaction-commit session [config])`

Commit the current transaction. A transaction must be in progress when this
method is called. If sesssion-commit-transaction returns #f, the transaction
was rolled back, not committed.

#### `(session-transaction-rollback session [config])`

Roll back the current transaction. A transaction must be in progress when this
method is called. All cursors are reset.


### cursor

#### `(cursor-open session uri config) -> <cursor>`

Open a new cursor on a data source or duplicate an existing cursor.

An existing cursor can be duplicated by passing it as the `to_dup` parameter
and setting the uri parameter to `#nil`. Cursors being duplicated must have a
key set, and successfully duplicated cursors are positioned at the same place
in the data source as the original.

To reconfigure a cursor, duplicate it with a new configuration value

Cursor handles should be discarded by calling `cursor-close`.

Cursors capable of supporting transactional operations operate in the context
of the current transaction, if any.

`session-transaction-rollback` implicitly resets all cursors.

Cursors are relatively light-weight objects but may hold references to
heavier-weight objects; applications should re-use cursors when possible,
but instantiating new cursors is not so expensive that applications need
to cache cursors at all cost.

`uri` is the data source on which the cursor operates; cursors are usually
opened on tables, however, cursors can be opened on any data source,
regardless of whether it is ultimately stored in a table. Some cursor
types may have limited functionality (for example, they may be read-only
or not support transactional updates). See **Data Sources** for more
information.

#### `(cursor-key-set cursor . key)`

Set the key for the next operation. If an error occurs during this operation,
a flag will be set in the cursor, and the next operation to access the value
will fail. This simplifies error handling in applications.

`key` must consistent with the format of the current object key.

#### `(cursor-value-set cursor key)`

Set the key for the next operation. If an error occurs during this operation,
a flag will be set in the cursor, and the next operation to access the key will
fail. This simplifies error handling in applications.

`key` must consistent with the format of the current object value.

#### `(cursor-key-ref cursor) -> list`


Get the key for the current record. The returned value is a bytevector that can
be unpacked using the correct key format string of the associated object.

#### `(cursor-value-ref cursor) -> list`

Get the value for the current record. The returned value is a bytevector that
can be unpacked using the correct key format string of the associated object.

#### `(cursor-next cursor) -> boolean`

Move the cursor to the next record. Returns #f if there is no more records.

#### `(cursor-previous cursor) -> boolean`

Move the cursor to the previous record. Returns #f if there is no more records.

#### `(cursor-reset cursor) -> boolean`

Reset the position of the cursor. Any resources held by the cursor are released,
and the cursor's key and position are no longer valid. A subsequent iteration
with `cursor-next` will move to the first record, or with `cursor-prev` will
move to the last record.

#### `(cursor-search cursor) -> boolean`

On sucess move the cursor to the record matching the key. The key must first
be set.

To minimize cursor resources, the `cursor-reset` method should be called as soon
as the record has been retrieved and the cursor no longer needs that position.

#### `(cursor-search-near cursor) -> -1, 0, 1 or #f`

Return the record matching the key if it exists, or an adjacent record.
An adjacent record is either the smallest record larger than the key or the
largest record smaller than the key (in other words, a logically adjacent key).
The key must first be set.

On success, the cursor ends positioned at the returned record; to minimize
cursor resources, the cursor-reset method should be called as soon as the record
has been retrieved and the cursor no longer needs that position.

#### `(cursor-insert cursor) -> boolean`

Insert a record and optionally update an existing record.

If the cursor was configured with overwrite=true (the default), both the key
and value must be set; if the record already exists, the key's value will be
updated, otherwise, the record will be inserted.

If the cursor was configured with overwrite=false, both the key and value must
be set and the record must not already exist; the record will be inserted.

If a cursor with record number keys was configured with append=true (not the
default), the value must be set; a new record will be appended and the record
number set as the cursor key value.

The cursor ends with no position, and a subsequent call to the cursor-next
`cursor-prev` method will iterate from the beginning (end) of the table.

Inserting a new record after the current maximum record in a fixed-length bit
field column-store (that is, a store with an r type key and t type value) may
implicitly create the missing records as records with a value of 0.

When loading a large amount of data into a new object, using a cursor with the
bulk configuration string enabled and loading the data in sorted order will be
much faster than doing out-of-order inserts. See Bulk-load for more information.

The maximum length of a single column stored in a table is not fixed (as it
partially depends on the underlying file configuration), but is always a small
number of bytes less than 4GB.

#### `(cursor-update cursor) -> boolean`

Update a record and optionally insert an existing record.

If the cursor was configured with overwrite=true (the default), both the key and
value must be set; if the record already exists, the key's value will be
updated, otherwise, the record will be inserted.

If the cursor was configured with overwrite=false, both the key and value must
be set and the record must already existe; the record will be updated.

On success, the cursor ends positioned at the modified record; to minimize
cursor resources, the cursor-reset method should be called as soon as the
cursor no longer needs that position.

The maximum length of a single column stored in a table is not fixed (as it
partially depends on the underlying file configuration), but is always a small
number of bytes less than 4GB.

#### `(cursor-remove cursor) -> boolean`

Remove a record. The key must be set.

If the cursor was configured with overwrite=true (the default), the key must be
set; the key's record will be removed if it exists, no error will be returned if
the record does not exist.

If the cursor was configured with overwrite=false, the key must be set and the
key's record must exist; the record will be removed.

Removing a record in a fixed-length bit field column-store (that is, a store
with an `r` type key and t type value) is identical to setting the record's
value to 0.

On success, the cursor ends positioned at the removed record; to minimize cursor
resources, the cursor-reset method should be called as soon as the cursor no
longer needs that position.

#### `(cursor-close cursor) -> boolean`

Close the cursor. This releases the resources associated with the cursor handle.
Cursors are closed implicitly by ending the enclosing connection or closing the
session in which they were opened.

## Packing

You will need to use packing function directly if you build some kind of generic
database. If you use wiredtiger like a regular RDBMS by setting the correct
format for each column guile-wiredtiger will take care of packing and unpacking.

### high level

`(scm->bytevector scm)` and `(bytevector->scm bv)` are both built to work
together on column with `u` format. It makes possible to use a column to store
anykind of scheme value and have an order on them. Right now the order is the
following:

- exact integers which can be signed are ordered using the integer order
- strings comes afters integers
- anything else. Those have not particular order. And depends guile
  serialization format.

The disavantage of this column is that it consume an extra integer to store the
type of the object.

In the future more types will be supported. And this format will be integrated
into cursor procedures so that the conversion is transparent for the user making
it much easier to work with this format.

#### `(scm->bytevector scm) -> bytevector`

#### `(bytevector->scm bv) -> scm`

### low level

There is also `pack` and `unpack` procedures tailored to keep the
database ordered. They are used internally in `cursor-key-set`,
`cursor-value-set`, `cursor-key-ref`, `cursor-value-ref`.

Both `pack` and `unpack` of this function do not check for the validity of
their arguments as such it can fail in non-obvious way and can be the reason
why you program doesn't work.

```
(pack fmt . args)) -> bytevector
```

`fmt` is a configuration string that must match the underlying key or value
record format. It only support integral types `bBhHiIlLqQr` and variable length
strings `S`.See [format types for more information](http://source.wiredtiger.com/2.6.1/schema.html#schema_format_types).
`args` must *match* `fmt`.

```
(unpack fmt bytevector)) -> list
```

`fmt` is a configuration string that must match the underlying record format. It
only support integral types `bBhHiIlLqQr` and variable length strings `S`. See
[format types](http://source.wiredtiger.com/2.6.1/schema.html#schema_format_types)
for more information.

# Support

For any question reach us via IRC at **irc.freenode.net#guile** for support. Or
via the [users mailling list](https://www.gnu.org/software/guile/community.html).
