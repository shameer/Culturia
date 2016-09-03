## Guile Wiredtiger

Wiredtiger is a ordered key/value store written in C licensed gplv2 or
gplv3.  It's some what the successor of Oracle Berkeley Database
(bsddb). It's similar in principles to leveldb but faster. It allows
to build your own database easily.

It's a ACID, NoSQL, networkless, with multilpe columns table and
automatic indices, it has multithread support.

Here follow the documentation of that module which follows closely
wiredtiger API. It's highly recommended to get started to have a look
at
[wiredtiger's schema](http://source.wiredtiger.com/develop/schema.html)
documentation.

### Reference API

A lot of the API is available, what remains can be the subject of
patches :)

The following documentation doesn't cover all of wiredtiger, it is
best to have a look at
[wiredtiger manual](http://source.wiredtiger.com/develop/index.html)
too.

They are three objects in guile-wiredtiger:

- `<connection>` a represent a particular connection to the wiredtiger
  engine. ACID is not supported across several instance of
  `<connection>`.

- `<session>` has a `<connection>` as parent. It's not threadsafe.

- `<cursor>` has a `<session>` as parent. It's not threadsafe.

#### <connection>

##### (connection-open home config) -> connection

Open a connection to a database. Most applications will open a single
connection to a database. A connection can be shared among several
threads. There is no support for ACID transactions between several
connections.

`HOME` must be the path to the database home directory, the pointed
directory must exist or wiredtiger will throw a `wiredtiger`
exception.

See
[official documentation](http://source.wiredtiger.com/develop/group__wt.html#ga9e6adae3fc6964ef837a62795c7840ed)
to know which options can be passed in `CONFIG`.

Example:

```scheme
(connection-open "/tmp/" "create,cache_size=500M")
```

##### (connection-close connection [config]) -> boolean

Close connection. Any open sessions or cursors will be
closed. `CONFIG` optional argument can be `leak_memory` to not free
memory during close.

##### (connection-add-collator connection name format proc)

**This is unstable and will hopefully be subject to changes**

Add `PROC` as a custom collation function named `NAME` against
`CONNECTION`. It can be later referenced in `session-create`
configuration using `NAME`.

Basically a collator procedure allows to customize the function used
to order records in a table. It's useful in situations where default
lexicographic ordering is not what you want. You should expect a slow
down during insert. For instance, a quick solution to pack bignums is
to use `write` and use a string column to store it but this will lead
to a not properly ordered table since `(string<? "10" "2") -> #true`,
instead using a custom collation you can use a Guile number comparison
function to provide the correct ordering.

The interface is a bit different from wiredtiger. (FIXME: explain why)

`FORMAT` must a be a wiredtiger format string it will be used to
unpack keys before passing them to `PROC`.

`PROC` must be a comparison procedure with the following signature:

```scheme
(proc key other) -> number
```

`PROC` must return to `-1` if `key < other`, `0` if `key == other`,
`1` if `key > other`.

It's recommended to read
[documentation about custom collators](http://source.wiredtiger.com/develop/custom_collators.html).


#### <session>

All data operations are performed in the context of a session. This
encapsulates the thread and transactional context of the operation.

Thread safety: A session is not usually shared between threads, see
[Multithreading](http://source.wiredtiger.com/develop/threads.html)
for more information. You can create a session in a thread and pass it
to another, but you can't concurrently use the session from different
threads.

##### (session-open connection [config]) -> <session>

Open a session against `CONNECTION`. You will most likely want to call
this procedure.

`CONFIG` can be used to provide the isolation level for the whole
session:

- `read-uncommited` transactions can see changes made by other
  transactions before those transactions are committed. Dirty reads,
  non-repeatable reads and phantoms are possible. (default value)

- `read-commited` transactions cannot see changes made by other
  transactions before those transactions are committed. Dirty reads
  are not possible; non-repeatable reads and phantoms are
  possible. Committed changes from concurrent transactions become
  visible when no cursor is positioned in the read-committed
  transaction.

- `snapshot` transactions read the versions of records committed
  before the transaction started. Dirty reads and non-repeatable reads
  are not possible; phantoms are possible.

If you don't know what you are doing, use `snapshot`.

Example:

```scheme
(session-open connection "isolation=snapshot")
```

##### (session-close session)

Close the session handle. This will release the resources associated with the
session handle, including rolling back any active transactions and closing any
cursors that remain open in the session.

##### (session-create session name config)

Create a table, column group, index or file.

`NAME` the URI of the object to create, such as `"table:stock"`. For a
description of URI formats see [Data Sources](http://source.wiredtiger.com/develop/data_sources.html).

`CONFIG` configures the object,
cf. [official documentation](http://source.wiredtiger.com/develop/struct_w_t___s_e_s_s_i_o_n.html#a358ca4141d59c345f401c58501276bbb)
for details.

In guile-wiredtiger, not all formats are supported. Patch welcome!

Example:

```
(session-create session "table:magic-numbers" "key_format=Q,value_format=S")
```

##### (session-transaction-begin session [config])

Start a transaction. A transaction remains active until ended with
`session-transaction-commit` or
`session-transaction-rollback`. Operations performed on cursors
capable of supporting transactional operations that are already open
in this session, or which are opened before the transaction ends, will
operate in the context of the transaction.

`session-transaction-begin` will throw a `wiredtiger` exception if a
transaction is already in progress in the session.

`CONFIG` can take several arguments
cf. [official documentation](http://source.wiredtiger.com/develop/struct_w_t___s_e_s_s_i_o_n.html#a7e26b16b26b5870498752322fad790bf).

##### (session-transaction-commit session [config])

Commit the current transaction. A transaction must be in progress when
this method is called. Throw a `wiredtiger` exception if the
transaction was rolledback.

##### (session-transaction-rollback session [config])

Rollback the current transaction. A transaction must be in progress when this
methods called. All cursors are reset.

#### <cursor>

##### (cursor-open session uri config) -> <cursor>

Open a new cursor on a data source or duplicate an existing cursor.

Cursor handles should be discarded by calling `cursor-close`.

Cursors capable of supporting transactional operations operate in the
context of the current transaction, if any.

`session-transaction-rollback` implicitly resets all cursors.

Cursors are relatively light-weight objects but may hold references to
heavier-weight objects; applications should re-use cursors when
possible, but instantiating new cursors is not so expensive that
applications need to cache cursors at all cost.

`URI` is the data source on which the cursor operates; cursors are
usually opened on tables, however, cursors can be opened on any data
source, regardless of whether it is ultimately stored in a table. Some
cursor types may have limited functionality (for example, they may be
read-only or not support transactional updates). See
[Data Sources](http://source.wiredtiger.com/develop/data_sources.html)
for more information.

`CONFIG` can take several arguments, cf. 
[official documentation](http://source.wiredtiger.com/develop/struct_w_t___s_e_s_s_i_o_n.html#afb5b4a69c2c5cafe411b2b04fdc1c75d)

##### (cursor-key-set cursor . key)

Set the key for the next operation. If an error occurs during this operation,
a flag will be set in the cursor, and the next operation to access the value
will fail. This simplifies error handling in applications.

`KEY` must consistent with the format of the current object key.

##### (cursor-value-set cursor value)

Set the value for the next operation. If an error occurs during this operation,
a flag will be set in the cursor, and the next operation to access the value will
fail. This simplifies error handling in applications.

`VALUE` must consistent with the format of the current object value.

##### (cursor-key-ref cursor) -> list

Get the key for the current record.

##### (cursor-value-ref cursor) -> list

Get the value for the current record.

##### (cursor-next cursor)

Move the cursor to the next record. Throw a `wiredtiger` exception if
there none.

##### (cursor-previous cursor)

Move the cursor to the previous record. Throw a `wiredtiger` exception if
there none.

##### (cursor-reset cursor)

Reset the position of the cursor. Any resources held by the cursor are released,
and the cursor's key and position are no longer valid. A subsequent iteration
with `cursor-next` will move to the first record, or with `cursor-prev` will
move to the last record.

##### (cursor-search cursor)

On sucess move the cursor to the record matching the key. The key must first
be set.

To minimize cursor resources, the `cursor-reset` method should be called as soon
as the record has been retrieved and the cursor no longer needs that position.

##### (cursor-search-near cursor) -> -1, 0, 1

Return the record matching the key if it exists, or an adjacent record.
An adjacent record is either the smallest record larger than the key or the
largest record smaller than the key (in other words, a logically adjacent key).
The key must first be set.

On success, the cursor ends positioned at the returned record; to minimize
cursor resources, the cursor-reset method should be called as soon as the record
has been retrieved and the cursor no longer needs that position.

##### (cursor-insert cursor)

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

##### (cursor-update cursor)

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

##### (cursor-remove cursor)

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

##### (cursor-close cursor) -> boolean

Close the cursor. This releases the resources associated with the cursor handle.
Cursors are closed implicitly by ending the enclosing connection or closing the
session in which they were opened.
