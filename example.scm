(use-modules (wiredtiger))

(define connection (pk (connection-open "/tmp/wt" "create")))
(define session (pk (session-open connection)))

;; create a table
(session-create session "table:nodes" "key_format=Q,value_format=SS")

;; open a cursor over that table
(define cursor (pk (cursor-open session "table:nodes")))

;; start a transaction and add a record
(session-transaction-begin session "isolation=\"snapshot\"")

(cursor-key-set cursor 42)
(cursor-value-set cursor "The one true number!" "magic happens")

(cursor-insert cursor)

(session-transaction-commit session)
(cursor-reset cursor)

;; retrieve record
(cursor-next cursor)
(pk (cursor-key-ref cursor))
(pk (cursor-value-ref cursor))
(cursor-close cursor)
(session-close session)
(connection-close connection)
