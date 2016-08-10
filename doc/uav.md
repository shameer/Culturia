# UAV

**U**ID **A**ttribute **V**alue database aka. uav is the versatile database written for Guile.

Other procedures exists for more complex situations...

## `(uav-open* path)`

Open a database at `PATH` and set a context for current dynamic state.

## `(with-transaction* e ...)`

Execute expressions inside a transaction.

## `(uav-debug)`

Display the content of the database

## `(uav-ref* uid)`

Retrieve the association that has `UID` as unique identifier.

## `(uav-add! assoc)`

Insert `ASSOC` inside the database and return its unique identifier.

## `(uav-del! uid)`

Delete all tuples having `UID` as unique identifier.

## `(uav-update! uid assoc)`

Update database with `ASSOC` with unique identifier `UID`.

## `(uav-index-ref attribute value)`

Return the list of unique identifiers which for which a
`(uid ATTRIBUTE VALUE)` tuple exists.

## `(query* value? ... :where ((u a v) ...)`

Return the list of `value? ...` that match the pattern `((u a v) ...`
where intermediate variables must be named with `??` suffix.

## client/server

The database server is simple read eval loop executed in the
environment of the `uav` module which includes all wiredtiger and uav
procedures. Otherwise said, the door is open for many many things which
includes evil things if you are not the one doing the eval...

**Don't expose the server to the outside world!**

That being said, there's two procedures that you must be aware.

First you can spawn a server using the `(uav-server path port)`
procedure.

Then you will be able to make queries against this server using the
`((uav-call port) proc . args)` where `PORT` is the same `PORT` passed
to `uav-server`. `PROC` must be *quoted lambda* taking as much
argument as there elements in `ARGS`.

For instance, if you do the following call client side:

```scheme
((uav-call 12345) '(lambda (x) (+ x x)) 2)
```

The following code is executed server side:

```scheme
(apply (eval proc (current-module)) args)
```

Which translates to calling `(lambda (x) (+ x x))` with `2` as argument.

And the result is returned to the client.

Easy enough.
