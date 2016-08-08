# Guile Wiredtiger

## wiredtiger

Wiredtiger is a ordered key/value store written in C licensed gplv2 or gplv3.
It's some what the successor of Oracle Berkeley Database (bsddb). It's similar
in principles to leveldb but faster. It allows to build your own database
easily.

It's a ACID, NoSQL, networkless, with automatic indices, multithread support.

Here follow the documentation of that module which follows closely wiredtiger
API. It's highly recommended to get started to have a look at
[wiredtiger's schema](http://source.wiredtiger.com/2.6.1/schema.html)
documentation.


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


### <connection>

#### (connection-open home config) -> connection

Open a connection to a database. Most applications will open a single connection
to a database. A connection can be shared among several threads. There is no
support ACID transactions between several connections.

Example:

```scheme
(connection-open "./databases/magic-numbers" "create,cache_size=500M")
```

`home` the path to the database home directory. The path must exists.

#### (connection-close connection [config]) -> boolean

Close connection. Any open sessions will be closed. config optional argument,
that can be `leak_memory` to not free memory during close.


### <session>

#### (session-open connection [config]) -> <session>

Open a session.

Example:

```scheme
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

#### (session-close session)

Close the session handle. This will release the resources associated with the
session handle, including rolling back any active transactions and closing any
cursors that remain open in the session.

All data operations are performed in the context of a session. This encapsulates
the thread and transactional context of the operation.

Thread safety: A session is not usually shared between threads, see
Multithreading for more information.

#### (session-create session name config)

Create a table, column group, index or file.

Example:

```scheme
(session-create session "table:magic-numbers" "key_format=i,value_format=S")
```

`name` the URI of the object to create, such as `"table:stock"`. For a
description of URI formats see **Data Sources**.

`config` configures the object.

In guile-wiredtiger, `key_format` and `key_value` only support integral types
`bBhHiIlLqQr` and variable length strings `S`. See format types for more
information.

#### (session-transaction-begin session [config])

Start a transaction.

#### (session-transaction-commit session [config])

Commit the current transaction. A transaction must be in progress when this
method is called. If sesssion-commit-transaction returns #f, the transaction
was rolled back, not committed.

#### (session-transaction-rollback session [config])

Roll back the current transaction. A transaction must be in progress when this
method is called. All cursors are reset.


### <cursor>

#### (cursor-open session uri config) -> <cursor>

Open a new cursor on a data source or duplicate an existing cursor.

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

#### (cursor-key-set cursor . key)

Set the key for the next operation. If an error occurs during this operation,
a flag will be set in the cursor, and the next operation to access the value
will fail. This simplifies error handling in applications.

`key` must consistent with the format of the current object key.

#### (cursor-value-set cursor key)

Set the key for the next operation. If an error occurs during this operation,
a flag will be set in the cursor, and the next operation to access the key will
fail. This simplifies error handling in applications.

`key` must consistent with the format of the current object value.

#### (cursor-key-ref cursor) -> list


Get the key for the current record. The returned value is a bytevector that can
be unpacked using the correct key format string of the associated object.

#### (cursor-value-ref cursor) -> list

Get the value for the current record. The returned value is a bytevector that
can be unpacked using the correct key format string of the associated object.

#### (cursor-next cursor) -> boolean

Move the cursor to the next record. Returns #f if there is no more records.

#### (cursor-previous cursor) -> boolean

Move the cursor to the previous record. Returns #f if there is no more records.

#### (cursor-reset cursor) -> boolean

Reset the position of the cursor. Any resources held by the cursor are released,
and the cursor's key and position are no longer valid. A subsequent iteration
with `cursor-next` will move to the first record, or with `cursor-prev` will
move to the last record.

#### (cursor-search cursor) -> boolean

On sucess move the cursor to the record matching the key. The key must first
be set.

To minimize cursor resources, the `cursor-reset` method should be called as soon
as the record has been retrieved and the cursor no longer needs that position.

#### (cursor-search-near cursor) -> -1, 0, 1 or #f

Return the record matching the key if it exists, or an adjacent record.
An adjacent record is either the smallest record larger than the key or the
largest record smaller than the key (in other words, a logically adjacent key).
The key must first be set.

On success, the cursor ends positioned at the returned record; to minimize
cursor resources, the cursor-reset method should be called as soon as the record
has been retrieved and the cursor no longer needs that position.

#### (cursor-insert cursor) -> boolean

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

#### (cursor-update cursor) -> boolean

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

#### (cursor-remove cursor) -> boolean

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

#### (cursor-close cursor) -> boolean

Close the cursor. This releases the resources associated with the cursor handle.
Cursors are closed implicitly by ending the enclosing connection or closing the
session in which they were opened.
