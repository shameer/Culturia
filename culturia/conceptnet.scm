;; guile-concepnet - 0.1 - 2015/08/09

;; Copyright © 2015 Amirouche BOUBEKKI <amirouche@hypermove.net>

;; guile-concepnet is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.

;; guile-concepnet is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with guile-concepnet.  If not, see <http://www.gnu.org/licenses/>
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

(use-modules (msgpack))

(use-modules (srfi srfi-1))  ;; lists
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-field

(use-modules (rnrs bytevectors))

(use-modules (ice-9 optargs))  ;; define*
(use-modules (ice-9 receive))  ;; receive
(use-modules (ice-9 match))  ;; match

(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (culture))

;;;
;;; Guile helpers
;;;

;; match helper
(define-syntax-rule (match-let (value query) e ...)
  (match query (value e ...)))


(define connection (connection-open "db/" "create"))
(define session (session-open connection))
(apply session-create* (cons session *culture*))
(define context (apply context-open (cons connection *culture*)))

(define iterator 0)
(define step 1)

(define (iter)
  (if (equal? iterator 100000)
      (begin (set! iterator 0) (pk 'step step) (set! step (1+ step)))
  (set! iterator (1+ iterator))))



(define (add entry)
  (atom-insert! (create-atom entry) context))

(define path "/media/amirouche/data/data/data/assertions/part_0")

(let next-file ((index 0))
  (let* ((filename (pk (string-append path (number->string index) ".msgpack")))
         (file (open filename  O_RDONLY)))
    (let next-entry ((entry (get-unpack file)))
      (if (eof-object? entry)
          (when (not (equal? index 0))
              (begin (close file) (set! step 0) (next-file (1+ index))))
          (begin (iter) (add entry) (next-entry (get-unpack file)))))))


(connection-close connection)
