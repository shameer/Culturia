;;; webui - Web interface for Culturia
;;; Copyright © 2015  Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2014  David Thompson <davet@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
(define-module (webui))

(use-modules (web server))
(use-modules (web http))
(use-modules (web request))
(use-modules (web response))
(use-modules (web server))
(use-modules (web uri))

(use-modules (srfi srfi-1))

(use-modules (ice-9 match))
(use-modules (ice-9 binary-ports))

(use-modules (sxml simple))

(use-modules (wiredtigerz))


;;; helpers


(define file-mime-types
  '(("css" . (text/css))
    ("js" . (text/javascript))
    ("png" . (image/png))))


(define (file-extension file-name)
  (last (string-split file-name #\.)))


(define (directory? filename)
  (string=? filename (dirname filename)))


(define (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))


(define (render-html sxml)
  (values '((content-type . (text/html)))
          (lambda (port)
            (sxml->xml sxml port))))


(define (not-found uri)
  (values (build-response #:code 404)
          (string-append "Resource not found: " uri)))


;;; user code

(define template
  (lambda (title body)
    `(html
      (head
       (meta (@ (charset "utf-8")))
       (title ,(string-append title " — Culturia"))
       (link (@ (rel "stylesheet") (href "/static/normalize.css")))
       (link (@ (rel "stylesheet") (href "/static/main.css"))))
       (body
        (div (@ (id "container"))
             (h1 (a (@ (href "/")) "Culturia"))
             ,body
             )))))


(define (render-static-asset path)
  (let ((file-name (string-join (cons* (dirname (current-filename)) "webui" "static" path) "/")))
    (if (and (file-exists? file-name)
             (not (directory? file-name)))
        (values `((content-type . ,(assoc-ref file-mime-types
                                              (file-extension file-name))))
                (call-with-input-file file-name get-bytevector-all))
        (not-found (string-join (cons "static" path) "/" 'prefix)))))


(define (handler request request-body)
  (match (request-path-components request)
    (("hello") (render-html (tempalte "hello" "hello")))
    (("bye") (render-html (template "bye" "bye")))    
    (() (render-html (template "index" "index")))
    (("static" path ...) (render-static-asset path))
    (_ (render-html (template "dunno" "dunno")))))


(use-modules (ice-9 receive))

(run-server handler)


;; ;;;
;; (define (handler request request-body)
;;   ((delegate index
;;              home
;;              notes-app
;;              404) (make-context request request-body)))

;; ;;; for instance to define apps one can simply do

;; (define notes-app (delegate note-index
;;                             note-home
;;                             note-add
;;                             note-edit
;;                             note-view                            
;;                             note-delete))

;; ;;; views have the responsability to "dispatch" more precisly it accepts
;; ;;; or reject the request, if it's rejected, the next view from the previous
;; ;;; delegate try to consume the request etc... until the delegate reject the request
;; ;;; it goes back to previous view

;; ;;; that's where things are difficult to express

;; (define $sub-path-match$ (list "note" "view" 'id))

;; (define (note-view context)
;;   (call-only-if-match context $sub-path-match$
;;                       (lambda (context)  ;; new context with match values from above
;;                                          ;; context has an extra key 'id
;;                         (render-html (template "foo" "bar")))))
          

;; ;;; one can also compose `delegate` and `call-only-if-match`

;; (run-server handler)
