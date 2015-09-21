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

(use-modules (tpldb))

;;;
;;; Guile helpers
;;;

;; match helper
(define-syntax-rule (match-let (value query) e ...)
  (match query (value e ...)))


(define db (create-tpldb "/data/conceptnet/db/"))


(define iterator 0)
(define step 1)

(define (iter)
  (if (equal? iterator 100000)
      (begin (gc) (set! iterator 0) (pk 'step step) (set! step (1+ step)))
  (set! iterator (1+ iterator))))



(define (add db entry)
  (tpldb-begin db)
  (for-each (lambda (name)
         (let ((uid (cdr (assoc "id" entry))))
           (if (not (null? (cdr (assoc name entry))))
                 (tpldb-add db uid name (cdr (assoc name entry))))))
       (list "end"
             "dataset"
             "source_uri"
             "rel"
             "context"
             "features"
             "sources"
             "license"
             "start"
             "surfaceText"
             "weight"
             "uri"))
  (tpldb-commit db))

(let next-file ((index 0))
  (let* ((filename (pk (string-append "/data/conceptnet/part_0" (number->string index) ".msgpack")))
         (file (open filename  O_RDONLY)))
    (let next-entry ((entry (get-unpack file)))
      (if (eof-object? entry)
          (if (not (equal? index 7))
              (begin (close file) (set! step 0) (next-file (1+ index))))
          (begin (iter) (add db entry) (next-entry (get-unpack file)))))))


;; (define iav (tpldb-iav db))
;; (define avi (tpldb-avi db))

;; (tpldb-iav-map db pk "" "")

;; (define relations-types (make-hash-table))
;; (tpldb-avi-map db
;;                  (lambda (attribute value identifier)
;;                    (hash-set! relations-types value #true))
;;                  "rel"
;;                  #vu8()
;;                  "")
;; (hash-for-each (lambda (key value)
;;                  (pk key))
;;                relations-types)
;; ;;; ("/r/CompoundDerivedFrom")
;; ;;; ("/r/Attribute")
;; ;;; ("/r/CreatedBy")
;; ;;; ("/r/DefinedAs")
;; ;;; ("/r/DerivedFrom")
;; ;;; ("/r/Causes")
;; ;;; ("/r/AtLocation")
;; ;;; ("/r/CausesDesire")
;; ;;; ("/r/CapableOf")
;; ;;; ("/r/Antonym")


(tpldb-close db)
