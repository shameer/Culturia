;;;
;;; html module taken from culturia
;;;
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;;
;;; html.scm is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; html.scm is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with html.scm.  If not, see <http://www.gnu.org/licenses/>.

(define-module (html))

(use-modules (ice-9 rdelim))
(use-modules (sxml simple))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 hash-table))
(use-modules (srfi srfi-1))

(use-modules (web uri))
(use-modules ((sxml xpath) #:renamer (symbol-prefix-proc 'sxml:)))

(use-modules (htmlprag))


;;;
;;; sxml->html
;;;

(define %void-elements
  '(area
    base
    br
    col
    command
    embed
    hr
    img
    input
    keygen
    link
    meta
    param
    source
    track
    wbr))

(define (void-element? tag)
  "Return #t if TAG is a void element."
  (pair? (memq tag %void-elements)))

(define %escape-chars
  (alist->hash-table
   '((#\" . "quot")
     (#\& . "amp")
     (#\' . "apos")
     (#\< . "lt")
     (#\> . "gt"))))

(define (string->escaped-html s port)
  "Write the HTML escaped form of S to PORT."
  (define (escape c)
    (let ((escaped (hash-ref %escape-chars c)))
      (if escaped
          (format port "&~a;" escaped)
          (display c port))))
  (string-for-each escape s))

(define (object->escaped-html obj port)
  "Write the HTML escaped form of OBJ to PORT."
  (string->escaped-html
   (call-with-output-string (cut display obj <>))
   port))

(define (attribute-value->html value port)
  "Write the HTML escaped form of VALUE to PORT."
  (if (string? value)
      (string->escaped-html value port)
      (object->escaped-html value port)))

(define (attribute->html attr value port)
  "Write ATTR and VALUE to PORT."
  (format port "~a=\"" attr)
  (attribute-value->html value port)
  (display #\" port))

(define (element->html tag attrs body port)
  "Write the HTML TAG to PORT, where TAG has the attributes in the
list ATTRS and the child nodes in BODY."
  (format port "<~a" tag)
  (for-each (match-lambda
             ((attr value)
              (display #\space port)
              (attribute->html attr value port)))
            attrs)
  (if (and (null? body) (void-element? tag))
      (display " />" port)
      (begin
        (display #\> port)
        (for-each (cut sxml->html <> port) body)
        (format port "</~a>" tag))))

(define (doctype->html doctype port)
  (format port "<!DOCTYPE ~a>" doctype))

(define* (sxml->html tree #:optional (port (current-output-port)))
  "Write the serialized HTML form of TREE to PORT."
  (match tree
    (() *unspecified*)
    (('doctype type)
     (doctype->html type port))
    (((? symbol? tag) ('@ attrs ...) body ...)
     (element->html tag attrs body port))
    (((? symbol? tag) body ...)
     (element->html tag '() body port))
    ((nodes ...)
     (for-each (cut sxml->html <> port) nodes))
    ((? string? text)
     (string->escaped-html text port))
    ;; Render arbitrary Scheme objects, too.
    (obj (object->escaped-html obj port))))

(export sxml->html)

;;;
;;; extracting links
;;;

(define (extract-href sxml)
  (map cadr ((sxml:sxpath '(// a @ href)) sxml)))

(define (url-domain url)
  (string-take url
               (cond
                ((string-prefix? "http://" url)
                 (let ((has-slash (string-index (string-drop url 7) #\/)))
                   (if has-slash
                       (+ 7 has-slash)
                       (string-length url))))
                ((string-prefix? "https://" url)
                 (let ((has-slash (string-index (string-drop url 8) #\/)))
                   (if has-slash
                       (+ 8 has-slash)
                       (string-length url))))
                (else (string-length url)))))

(define (proprify url)
  "Remove extra slash in URL path"
  (let* ((path (string-split (uri-path (string->uri url)) #\/))
         (clean (lambda (string)
                  (not (or (equal? string "") (equal? string ".")))))
         (path (filter clean path))
         (join (lambda (lst) (string-join lst "/")))
         (make-url (lambda (path)
                     (string-append (url-domain url) "/" (join path)))))
    (make-url (let loop ((path path) (out '()))
                (if (null? path)
                    (reverse out)
                    (if (equal? (car path) "..")
                        (loop (cdr path) (cdr out))
                        (loop (cdr path) (cons (car path) out))))))))

(define* ((href->url original-document-url) href)
  (proprify (cond
             ((string-prefix? "http" href) href)
             ((string-prefix? "//" href)
              (if (string-prefix? "http://" original-document-url)
                  (string-append "http:" href)
                  (string-append "https:" href)))
             ((string-prefix? "/" href)
              (string-append (url-domain original-document-url) href))
             ;; ./foo/bar/baz and foo/bar/baz
             (else
              (if (string-suffix? "/" original-document-url)
                  (string-append original-document-url href)
                  (string-append (url-domain original-document-url)
                                 "/"
                                 (dirname (uri-path (string->uri original-document-url)))
                                 "/"
                                 href))))))

(define (unsupported-href href)
  (not (or (string-prefix? "#" href)
           (string-prefix? "mailto:" href))))

(define-public (extract-links original-document-url string)
  (let ((hrefs (extract-href (html->sxml string))))
    (delete-duplicates
     (map (href->url original-document-url)
          (filter unsupported-href hrefs)))))
