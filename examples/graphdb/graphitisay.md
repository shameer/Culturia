# graphitisay

graphitisay is graph database written in guile using wiredtiger.

## graph

This is the first object you create. You won't need to use it much outside the initialisation of the
database.

### `(create-graph dir [indices])`

Create a graph inside `DIR` directory. The graph will use the key names found in `indices` to index
edges and vertices.

### `(graph-close graph)`

Close the graph database `graph`.

## txn

The primary object of graphitisay.

### `(txn-begin graph) -> <transaction>`

### `(txn-commit graph)`

### `(txn-abort graph)`

### `(txn-create-node txn label [properties])`

### `(txn-node-ref txn uid)`

### `(txn-nodes-label txn label)`

### `(txn-nodes-all txn)`

### `(txn-create-edge! txn start label end [properties])`

### `(txn-edge-ref txn uid)`

### `(txn-edge-all txn)`

## node

### `(node-uid node)`

Return the unique identifier of `node`.

### `(node-label node)`

Return the label of `node`.

### `(node-outgoings-uids node)`

Return the outgoings edges unique identifiers of `node`.

### `(node-outgoings node)`

Return the outgoings edges of `node`.

### `(node-incomings-uids node)`

Return the incomings edges unique identifiers of `node`.

### `(node-incomings node)`

Return the incomings edges of `node`.

### `(node-properties node)`

Return the properties of node.

### `(node-properties-set node key value)`

Set `key` to `value` in `node` properties.

### `(node-properties-ref node key)`

Return the value associated with `key` in `node` properties.

### `(node-properties-del node key)`

Remove the key/value pair associated with `key` in `node` properties.

### `(node-save node)`

Save changes done to node.

## Edge

### `(edge-uid node)`

### `(edge-start edge)`

### `(edge-end edge)`

### `(edge-properties edge)`

### `(edge-properties-set edge key value)`

### `(edge-properties-ref edge key)`

### `(edge-properties-del edge del)`

### `(edge-save edge)`
