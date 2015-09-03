(use-modules (wiredtiger))

(define connection (pk (connection-open "/tmp/wt" "create")))
(define session (pk (session-open connection)))

;; create a table
(session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
(session-create session "index:nodes:index" "columns=(a, b)")

;; open a cursor over that table
(define cursor (pk (cursor-open session "table:nodes")))


;;
(pk 'set)
(session-transaction-begin session "isolation=\"snapshot\"")

(cursor-key-set cursor 42)
(cursor-value-set cursor "aaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbb")

(pk 'key (cursor-key-ref cursor))
(pk 'value (cursor-value-ref cursor))

(cursor-insert cursor)

(session-transaction-commit session)
(cursor-reset cursor)

;; 
(pk 'ref)
(cursor-next cursor)
(pk 'key (cursor-key-ref cursor))
(pk 'value (cursor-value-ref cursor))


(pk 'index)
(define index (cursor-open session "index:nodes:index"))
(cursor-next index)
(pk 'index-key (cursor-key-ref index))
(pk 'index-value (cursor-value-ref index))



(cursor-close cursor)
(session-close session)
(connection-close connection)
