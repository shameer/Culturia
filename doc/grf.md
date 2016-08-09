## grf

`grf` is graph layer on top of wiredtiger. It's written in way
where you have to know how wiredtiger works to be able to use, this is
on purpose.

### *grf*

This defines the tables with indices used by grf. You have to
use it in places where `wiredtigerz` expects a list of configuration.

For instance:

```
>>> (apply wiredtiger-open* (cons "/tmp" *grf*"))
```

This will return two values, the `<connection>` used to create the
database and a `<context>` (which can not be shared among threads).

### Vertex

```
>>> (define-record-type* <vertex> uid label assoc)
```

Vertex record holds most often accessed data. You can use the following
procedures to access its attributes:

- `vertex-uid`

- `vertex-label`

- `vertex-assoc`

`<vertex>` is mutable.

To retrieve outgoings edges use the following procedures:

- `(vertex-outgoings context uid)` returns the list of outgoings edges ie.
  edges that starts at the vertex with `UID` as identifier.

And for incomings edges:

- `(vertex-incomings context uid)` returns the list of incomings edges ie.
  edges that starts at the vertex with `UID` as identifier.

#### (vertex-ref context uid)

Retrieve vertex with `UID` unique identifier and return a `<vertex>` record for it.

#### (vertex-add context label assoc)

Create a new vertex with `LABEL` and `ASSOC` in the database and
return its unique identifier.

#### (vertex-save context vertex)

Persist to disk `VERTEX` attributes.

### Edge

```
(define-record-type* <edge> uid start label end assoc)
```

Edge record holds most often accessed data. You can use the
following procedures to access its attributes:

- `edge-uid`

- `edge-start` returns the vertex's uid where the edge starts 

- `edge-label`

- `edge-end` returns the vertex's uid where the edge ends

- `edge-assoc`

#### (edge-ref context uid)

Retrieve the edge with `UID` as unique identifier.

#### (edge-add context start label end assoc)

Create an edge with `START`, `LABEL`, `END` and `ASSOC` and return its
unique identifier.

#### (edge-save context edge)

Persist to disk `EDGE` attributes.
