;;; Guile Parser Combinators
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This module is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This module is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this module.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Monadic parser combinators.
;;
;;; Code:

(define-module (combinators)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:export (parse-result
            parse-result?
            parse-result-value
            parse-result-index
            parse-failure?
            parse-success?
            %parse-failure
            parse-fail
            parse-bind
            parse-return
            parse-lift
            parse

            parse-map
            parse-match
            parse-any
            parse-each
            parse-end
            parse-zero-or-more
            parse-one-or-more
            parse-up-to
            parse-maybe
            parse-any-char
            parse-char
            parse-char-set
            parse-string))

;;;
;;; Core
;;;

(define-record-type <parse-result>
  (parse-result value stream)
  parse-result?
  (value parse-result-value)
  (stream parse-result-stream))

(define (parse-failure? result)
  "Return #t if RESULT represents a failed parse."
  (not (parse-result-stream result)))

(define (parse-success? result)
  "Return #t if RESULT represents a successful parse."
  (stream? (parse-result-stream result)))

(define (parse-done? result)
  "Return #t if the remainder of RESULT is the empty stream."
  (stream-null? (parse-result-stream result)))

(define %parse-failure (parse-result #f #f))

(define (parse-fail stream)
  "Always fail to parse STREAM."
  %parse-failure)

(define (parse-bind proc parser)
  (lambda (stream)
    (match (parser stream)
      ((? parse-failure? _) %parse-failure)
      (($ <parse-result> value stream)
       ((proc value) stream)))))

(define (parse-return x)
  "Return a parser that always yields X as the parse result."
  (lambda (stream)
    (parse-result x stream)))

(define (parse-lift proc)
  "Return a procedure that wraps the result of PROC in a parser."
  (lambda args
    (parse-return (apply proc args))))

(define (string->stream str)
  "Convert the string STR into a stream of characters."
  (stream-map (lambda (i)
                (string-ref str i))
              (stream-range 0 (string-length str))))

(define* (parse parser obj #:optional (fail-value #f))
  "Parse the contents of OBJ with PARSER.  OBJ may be either a string, port,
or stream."
  (let ((stream (match obj
                  ;; Handle strings and ports as a convenience.
                  ((? string? str) (string->stream str))
                  ((? port? port) (port->stream port))
                  ((? stream? stream) stream))))
    (match (parser stream)
      ((or (? parse-failure?)
           (not (? parse-done?)))
       fail-value)
      (($ <parse-result> value _) value))))


;;;
;;; Extras
;;;

(define (parse-map proc parser)
  "Return a new parser that applies PROC to result of PARSER."
  (parse-bind (parse-lift proc) parser))

(define-syntax-rule (parse-match parser matchers ...)
  "Create a parser that applies pattern matching to transform the
successful results of PARSER using MATCHERS.  MATCHERS uses the (ice-9
match) pattern matching syntax."
  (parse-map (match-lambda matchers ...) parser))

(define (%parse-any . parsers)
  (lambda (stream)
    (let loop ((parsers parsers))
      (match parsers
        (() %parse-failure)
        ((parser . rest)
         (match ((force parser) stream)
           ((? parse-failure? _)
            (loop rest))
           (result result)))))))

(define (%parse-each . parsers)
  (lambda (stream)
    (let loop ((stream stream)
               (parsers parsers)
               (result '()))
      (match parsers
        (() (parse-result (reverse result) stream))
        ((parser . rest)
         (match ((force parser) stream)
           ((? parse-failure?) %parse-failure)
           (($ <parse-result> value stream)
            (loop stream rest (cons value result)))))))))

;; parse-any and parse-seach are special forms to abstract the lazy
;; evaluation used to handle right recursive grammars.
(define-syntax-rule (parse-any parser ...)
  "Create a disjunctive parser that succeeds if any of the input
parsers succeed."
  (%parse-any (delay parser) ...))

(define-syntax-rule (parse-each parser ...)
  "Create a sequential parser that returns a list of parse results if
all of the input parsers succeed."
  (%parse-each (delay parser) ...))

(define (parse-end stream)
  "Succeed with #t if STREAM is emtpy or fail otherwise."
  (if (stream-null? stream)
      (parse-result #t stream-null)
      %parse-failure))

(define (parse-zero-or-more parser)
  "Create a parser that applies PARSER as many times as it can before
failing and returns list of the successful parse results."
  (lambda (stream)
    (let loop ((stream stream)
               (result '()))
      (match (parser stream)
        ((? parse-failure?)
         (parse-result (reverse result) stream))
        (($ <parse-result> value stream)
         (loop stream (cons value result)))))))

(define (parse-one-or-more parser)
  "Return a parser that succeeds when PARSER can be successfully
applied at least once and returns a list of the successful parse
results."
  (lambda (stream)
    (let loop ((stream stream)
               (result '()))
      (match (parser stream)
        ((? parse-failure?)
         (if (null? result)
             %parse-failure
             (parse-result (reverse result) stream)))
        (($ <parse-result> value stream)
         (loop stream (cons value result)))))))

(define (parse-up-to n parser)
  "Create a parser that applies PARSER at most N times and returns a
list of the successful parse results."
  (lambda (stream)
    (let loop ((stream stream)
               (n n))
      (if (zero? n)
          '()
          (match (parser stream)
               ((? parse-failure?) '())
               (($ <parse-result> value stream)
                (cons value (loop stream (1- n)))))))))

(define* (parse-maybe parser #:optional (default #f))
  "Create a parser that returns the result of PARSER upon success, or
DEFAULT upon failure."
  (lambda (stream)
    (match (parser stream)
      ((? parse-failure?)
       (parse-result default stream))
      (result result))))

(define (parse-any-char stream)
  "Parse any single character or fail if STREAM is empty."
  (stream-match stream
    (() %parse-failure)
    ((head . tail)
     (parse-result head tail))))

(define (parse-char c)
  "Create a parser that succeeds when the next character in the stream
is C."
  (lambda (stream)
    (stream-match stream
      (() %parse-failure)
      ((head . tail)
       (if (equal? head c)
           (parse-result head tail)
           %parse-failure)))))

(define (parse-char-set char-set)
  "Create a parser that succeeds when the next character in the stream
is a member of CHAR-SET."
  (lambda (stream)
    (stream-match stream
      (() %parse-failure)
      ((char . tail)
       (if (char-set-contains? char-set char)
           (parse-result char tail)
           %parse-failure)))))

(define stream->string (compose list->string stream->list))

(define (parse-string str)
  "Create a parser that succeeds when the front of the stream contains
the character sequence in STR."
  (lambda (stream)
    (let ((input (stream->string (stream-take (string-length str) stream))))
      (if (string=? str input)
          (parse-result str (stream-drop (string-length str) stream))
          %parse-failure))))
