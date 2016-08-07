#! /bin/sh
# -*- scheme -*-
exec guile -L $(dirname $(dirname $0)) -e '(markdown)' -s $0 "$@"
!#
(define-module (markdown))

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))

(use-modules (combinators))


(define parse-newline
  (parse-map (lambda (x) #\newline)
             (parse-any (parse-char #\newline)
                        (parse-each (parse-char #\return) (parse-char #\newline)))))

(define (parse-when predicate parser)
  (lambda (stream)
    (match (predicate stream)
      ((? parse-success?) (parser stream))
      (_ %parse-failure))))

(define (parse-unless predicate parser)
  (lambda (stream)
    (match (predicate stream)
      ((? parse-failure?) (parser stream))
      (_ %parse-failure))))

(define (char->string parser)
  (parse-map (cut format #f "~c" <>) parser))

(define (chars->string parser)
  (parse-match parser
               ((char ...) (list->string char))))

(define (strings->string parser)
  (parse-match parser
               ((string ...) (string-concatenate string))))


(define *punctuation* (apply char-set (string->list "!\"#$%&\\'()*+,-./:;<=>?@[\\]^_`{|}~")))

(define parse-punctuation (char->string (parse-char-set *punctuation*)))

(define parse-word
  (chars->string
   (parse-one-or-more
    (parse-unless (parse-any parse-punctuation (parse-char #\space) parse-newline)
                  parse-any-char))))

(define parse-other
  (char->string (parse-any parse-punctuation (parse-char #\space))))

(define (make-hashtag-link hashtag)
  `(a (@ (class "hashtag")
         (href ,(string-append "/hashtag/" (string-drop hashtag 1))))
      ,hashtag))

(define (hashtag->sxml parser)
  (parse-map make-hashtag-link parser))

(define parse-hashtag
  (hashtag->sxml
   (strings->string
    (parse-each (char->string (parse-char #\#)) parse-word))))

(define (make-link url)
  `(a (@ (href ,url)) ,url))

(define (url->sxml parser)
  (parse-map make-link parser))

(define parse-nonspace
  (parse-unless (parse-char #\space) parse-any-char))

(define parse-url
  (url->sxml
   (strings->string
    (parse-each (parse-any (parse-string "http://") (parse-string "https://"))
                (chars->string (parse-one-or-more parse-nonspace))))))

(define parse-space
  (char->string (parse-char #\space)))

(define (italic->sxml parser)
  (parse-match parser ((#\* text #\*) `(i ,text))))

(define parse-italic
  (italic->sxml (parse-each (parse-char #\*)
                            (chars->string
                             (parse-one-or-more
                              (parse-unless (parse-char #\*) parse-any-char)))
                            (parse-char #\*))))

(define (code->sxml parser)
  (parse-match parser ((#\` text #\`) `(code ,text))))

(define parse-code
  (code->sxml (parse-each (parse-char #\`)
                            (chars->string
                             (parse-one-or-more
                              (parse-unless (parse-char #\`) parse-any-char)))
                            (parse-char #\`))))

(define (bold->sxml parser)
  (parse-match parser ((#\* #\* text #\* #\*) `(b ,text))))

(define parse-bold
  (bold->sxml (parse-each (parse-char #\*) (parse-char #\*)
                            (chars->string
                             (parse-one-or-more
                              (parse-unless (parse-char #\*) parse-any-char)))
                            (parse-char #\*) (parse-char #\*))))

(define (link->sxml parser)
  (parse-match parser ((#\[ text #\] #\( href #\)) `(a (@ (href ,href)) ,text))))

(define parse-link
  (link->sxml
   (parse-each (parse-char #\[)
               (chars->string (parse-one-or-more (parse-unless (parse-char #\]) parse-any-char)))
               (parse-char #\])
               (parse-char #\()
               (chars->string (parse-one-or-more (parse-unless (parse-char #\)) parse-any-char)))
               (parse-char #\)))))

(define (block->sxml parser)
  (parse-match parser ((e (#\newline #\newline)) e) ((e #t) e)))

(define (parse-block parser)
  (block->sxml
   (parse-each parser
               (parse-any (parse-each parse-newline parse-newline)
                          parse-end))))

(define (paragraph->sxml parser)
  (parse-map (cut cons 'p <>) parser))

(define (parse-once parser)
  (parse-unless (parse-each parser parser) parser))

(define parse-paragraph
  (parse-block
   (paragraph->sxml
    (parse-one-or-more
     (parse-any parse-link
                parse-bold
                parse-italic
                parse-code
                parse-url
                parse-hashtag
                parse-word
                parse-punctuation
                parse-space
                (parse-once parse-newline))))))

(define (header->sxml parser)
  (parse-match parser
               (("# " e) (cons 'h1 e))
               (("## " e) (cons 'h2 e))
               (("### " e) (cons 'h3 e))
               (("#### " e) (cons 'h4 e))
               (("##### " e) (cons 'h5 e))
               (("###### " e) (cons 'h6 e))))

(define parse-header
  (header->sxml (parse-each
                 (parse-any (parse-string "###### ")
                            (parse-string "##### ")
                            (parse-string "#### ")
                            (parse-string "### ")
                            (parse-string "## ")
                            (parse-string "# "))
                 (parse-block
                  (parse-one-or-more
                   (parse-any parse-bold
                              parse-code
                              parse-italic
                              parse-url
                              parse-hashtag
                              parse-word
                              parse-punctuation
                              parse-space))))))

(define (code-block->sxml parser)
  (parse-match parser
               (("```" #\newline (e ...) "```") (list 'pre (string-trim-right (list->string e))))))

(define parse-code-block
  (code-block->sxml (parse-block (parse-each
                                  (parse-string "```")
                                  parse-newline
                                  (parse-one-or-more (parse-unless (parse-string "```") parse-any-char))
                                  (parse-string "```")))))

(define-public (markdown string)
  (parse (parse-one-or-more (parse-any parse-code-block
                                       parse-header
                                       parse-paragraph
                                       ))
         string))


;;;
;;; sxml->html taken from Haunt
;;;
;;; Haunt --- Static site generator for GNU Guile
;;; Copyright Â© 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of Haunt.
;;;
;;; Haunt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Haunt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; SXML to HTML conversion.
;;
;;; Code:

(use-modules (ice-9 rdelim))
(use-modules (sxml simple))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 hash-table))


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


;;;
;;; main: markdown to html
;;;

(define-public (main args)
  (sxml->html `(html
                (head
                 (meta (@ (charset "utf-8")))
                 (title "guile-culturia")
                 (link (@ (rel "stylesheet")
                          (href "static/normalize.css")))
                 (link (@ (rel "stylesheet")
                          (href "https://fonts.googleapis.com/css?family=Gentium+Basic")))
                 (link (@ (rel "stylesheet")
                          (href "https://fonts.googleapis.com/css?family=Open+Sans")))
                 (link (@ (rel "stylesheet")
                          (href "static/main.css"))))
                (body
                 (h1 (a (@ (href "//hyperdev.fr/projects/culturia")) "hyperdev.fr/projects/culturia"))
                 (div (@ (id "container")) ,(markdown (read-string)))
                 (p (small "cc-by-nc-sa"))))))


;;;
;;; tests
;;;

(define (print . rest)
  (let ((template (reverse (cdr (reverse rest))))
        (parameters (car (reverse rest))))
    (let loop ((template template)
               (parameters parameters))
      (if (null? template)
          (newline)
          (if (procedure? (car template))
              (begin ((car template) (car parameters))
                     (loop (cdr template) (cdr parameters)))
              (begin (display (car template))
                     (loop (cdr template) parameters)))))))

(define (~s s) (format #true "~s" s))
(define (~a s) (format #true "~s" s))

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (print "* Checking " ~s (list title))
       (let* ((expected expected-result)
              (produced tested-expression))
         (if (not (equal? expected produced))
             (begin (print "Expected: " ~a (list expected))
                    (print "Computed: " ~a (list produced)))))))))


(when (or (getenv "CHECK") (getenv "CHECK_MARKDOWN"))

  (test-check "parse simple paragraph"
              (markdown "something")
              '((p "something")))

  (test-check "parse multiline paragraph"
              (markdown "something on first line
something else on second line")
              '((p "something" " " "on" " " "first" " " "line" #\newline
                   "something" " " "else" " " "on" " " "second" " " "line")))

  (test-check "parse multiple paragraph"
              (markdown "something

else")
              '((p "something") (p "else")))

  (test-check "parse multiple multiline paragraph"
              (markdown "some
thing

what
else")
              '((p "some" #\newline "thing")
                (p "what" #\newline "else")))

  (test-check "parse header and paragraph"
              (markdown "# header

paragraph over
several lines")
              '((h1 "header") (p "paragraph" " " "over" #\newline
                                 "several" " " "lines")))

    (test-check "foo [bar] baz"
              (markdown "foo [bar] baz")
              '((p "foo" " " "[" "bar" "]" " " "baz")))

    (test-check "link"
                (markdown "[bar](http://baz.com)")
                '((p (a (@ (href "http://baz.com")) "bar"))))

    (test-check "code block"
                (markdown "```
foo
bar
baz
```")
                '((pre "foo\nbar\nbaz")))

    (test-check "code block and text"
                (markdown "some paragraph

```
foo 
bar 
baz
```

some other paragraph
")
                '((p "some" " " "paragraph") (pre "foo \nbar \nbaz") (p "some" " " "other" " " "paragraph" #\newline))))
