# guile-wiredtiger 0.2


## Build your own database

- ACID
- NoSQL
- networkless
- automatic index
- multithread support
- ordered key/value store

And more...


## Informations

- Language: [GNU Guile](https://www.gnu.org/software/guile/)
- Tested with [wiredtiger](http://wiredtiger.com) **2.6.2**
- License: GPL2+ (same as wiredtiger)
- Join us at **irc.freenode.net#guile** for support.
- Mailling list: [guile-users](https://www.gnu.org/software/guile/community.html)
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

wiredtiger **does not work on 32 bits architectures**.

It was tested with wiredtiger `2.6.2` but might work with future versions.

You need to install wiredtiger with the usual
`./configure && make && make install` cli dance. If you prefer to use git:

```
git clone https://github.com/wiredtiger/wiredtiger.git
```

You also need a recent version of [GNU Guile](https://www.gnu.org/software/guile/) (tested with 2.0.11). It's available
in most GNU/Linux distributions.

## How to contribute

Send me a mail or patch. But before doing anything with guile-wiredtiger you are
warmly advised to come to #guile and discuss :)

## Support

For any question reach us via IRC at **irc.freenode.net#guile** for support. Or
via the [users mailling list](https://www.gnu.org/software/guile/community.html).
