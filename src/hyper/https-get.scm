o;;; (https-get)

;; Copyright (C) 2013, 2014 Aleix Conchillo Flaque <aconchillo@gmail.com>
;; Copyright (C) 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;
;; This file is based on guile-oauth's (oauth oauth1 utils).
;;
;; https-get is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; https-get is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-oauth; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; https-get for Guile

;;; Code:

(define-module (https-get)
  #:use-module (gnutls)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (web response))



(define add-weak-reference
  (let ((table (make-weak-key-hash-table)))
    (lambda (from to)
      "Hold a weak reference from FROM to TO."
      (hashq-set! table from to))))

(define (tls-wrap port server)
  "Return PORT wrapped in a TLS connection to SERVER.  SERVER must be a DNS
host name without trailing dot."
  (define (log level str)
    (format (current-error-port)
            "gnutls: [~a|~a] ~a" (getpid) level str))

  (let ((session (make-session connection-end/client)))

    ;; Some servers such as 'cloud.github.com' require the client to support
    ;; the 'SERVER NAME' extension.  However, 'set-session-server-name!' is
    ;; not available in older GnuTLS releases.  See
    ;; <http://bugs.gnu.org/18526> for details.
    (if (module-defined? (resolve-interface '(gnutls))
                         'set-session-server-name!)
        (set-session-server-name! session server-name-type/dns server)
        (format (current-error-port)
                "warning: TLS 'SERVER NAME' extension not supported~%"))

    (set-session-transport-fd! session (fileno port))
    (set-session-default-priority! session)
    (set-session-credentials! session (make-certificate-credentials))

    ;; Uncomment the following lines in case of debugging emergency.
    ;;(set-log-level! 10)
    ;;(set-log-procedure! log)

    (handshake session)
    (let ((record (session-record-port session)))
      ;; Since we use `fileno' above, the file descriptor behind PORT would be
      ;; closed when PORT is GC'd.  If we used `port->fdes', it would instead
      ;; never be closed.  So we use `fileno', but keep a weak reference to
      ;; PORT, so the file descriptor gets closed when RECORD is GC'd.
      (add-weak-reference record port)
      record)))

(define (ensure-uri uri-or-string)                ;XXX: copied from (web http)
  (cond
   ((string? uri-or-string) (string->uri uri-or-string))
   ((uri? uri-or-string) uri-or-string)
   (else (error "Invalid URI" uri-or-string))))

(define current-http-proxy
  ;; XXX: Add a dummy definition for Guile < 2.0.10; this is used in
  ;; 'open-socket-for-uri'.
  (or (and=> (module-variable (resolve-interface '(web client))
                              'current-http-proxy)
             variable-ref)
      (const #f)))

(define* (open-socket-for-uri uri-or-string #:key timeout)
  "Return an open input/output port for a connection to URI.  When TIMEOUT is
not #f, it must be a (possibly inexact) number denoting the maximum duration
in seconds to wait for the connection to complete; passed TIMEOUT, an
ETIMEDOUT error is raised."
  ;; Includes a fix for <http://bugs.gnu.org/15368> which affects Guile's
  ;; 'open-socket-for-uri' up to 2.0.11 included, uses 'connect*' instead
  ;; of 'connect', and uses AI_ADDRCONFIG.

  (define http-proxy (current-http-proxy))
  (define uri (ensure-uri (or http-proxy uri-or-string)))
  (define addresses
    (let ((port (uri-port uri)))
      (delete-duplicates
       (getaddrinfo (uri-host uri)
                    (cond (port => number->string)
                          (else (symbol->string (uri-scheme uri))))
                    (if (number? port)
                        (logior AI_ADDRCONFIG AI_NUMERICSERV)
                        AI_ADDRCONFIG))
       (lambda (ai1 ai2)
         (equal? (addrinfo:addr ai1) (addrinfo:addr ai2))))))

  (let loop ((addresses addresses))
    (let* ((ai (car addresses))
           (s  (with-fluids ((%default-port-encoding #f))
                 ;; Restrict ourselves to TCP.
                 (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
      (catch 'system-error
        (lambda ()
          (connect* s (addrinfo:addr ai) timeout)

          ;; Buffer input and output on this port.
          (setvbuf s _IOFBF)
          ;; If we're using a proxy, make a note of that.
          (when http-proxy (set-http-proxy-port?! s #t))
          s)
        (lambda args
          ;; Connection failed, so try one of the other addresses.
          (close s)
          (if (null? (cdr addresses))
              (apply throw args)
              (loop (cdr addresses))))))))

(define* (open-connection-for-uri uri #:key timeout)
  "Like 'open-socket-for-uri', but also handle HTTPS connections."
  (define https?
    (eq? 'https (uri-scheme uri)))

  (let-syntax ((with-https-proxy
                (syntax-rules ()
                  ((_ exp)
                   ;; For HTTPS URIs, honor 'https_proxy', not 'http_proxy'.
                   ;; FIXME: Proxying is not supported for https.
                   (let ((thunk (lambda () exp)))
                     (if (and https?
                              (module-variable
                               (resolve-interface '(web client))
                               'current-http-proxy))
                         (parameterize ((current-http-proxy #f))
                           (when (and=> (getenv "https_proxy")
                                        (negate string-null?))
                             (format (current-error-port)
                                     "warning: 'https_proxy' is ignored~%"))
                           (thunk))
                         (thunk)))))))
    (with-https-proxy
     (let ((s (open-socket-for-uri uri #:timeout timeout)))
       ;; Buffer input and output on this port.
       (setvbuf s _IOFBF %http-receive-buffer-size)

       (if https?
           (tls-wrap s (uri-host uri))
           s)))))

;; XXX: This is an awful hack to make sure the (set-port-encoding! p
;; "ISO-8859-1") call in `read-response' passes, even during bootstrap
;; where iconv is not available.
(module-define! (resolve-module '(web response))
                'set-port-encoding!
                (lambda (p e) #f))

;; XXX: Work around <http://bugs.gnu.org/13095>, present in Guile
;; up to 2.0.7.
(module-define! (resolve-module '(web client))
                'shutdown (const #f))

;; XXX: Work around <http://bugs.gnu.org/19840>, present in Guile
;; up to 2.0.11.
(unless (or (> (string->number (major-version)) 2)
            (> (string->number (minor-version)) 0)
            (> (string->number (micro-version)) 11))
  (let ((var (module-variable (resolve-module '(web http))
                              'declare-relative-uri-header!)))
    ;; If 'declare-relative-uri-header!' doesn't exist, forget it.
    (when (and var (variable-bound? var))
      (let ((declare-relative-uri-header! (variable-ref var)))
        (declare-relative-uri-header! "Location")))))

(define (resolve-uri-reference ref base)
  "Resolve the URI reference REF, interpreted relative to the BASE URI, into a
target URI, according to the algorithm specified in RFC 3986 section 5.2.2.
Return the resulting target URI."

  (define (merge-paths base-path rel-path)
    (let* ((base-components (string-split base-path #\/))
           (base-directory-components (match base-components
                                        ((components ... last) components)
                                        (() '())))
           (base-directory (string-join base-directory-components "/")))
      (string-append base-directory "/" rel-path)))

  (define (remove-dot-segments path)
    (let loop ((in
                ;; Drop leading "." and ".." components from a relative path.
                ;; (absolute paths will start with a "" component)
                (drop-while (match-lambda
                              ((or "." "..") #t)
                              (_ #f))
                            (string-split path #\/)))
               (out '()))
      (match in
        (("." . rest)
         (loop rest out))
        ((".." . rest)
         (match out
           ((or () (""))
            (error "remove-dot-segments: too many '..' components" path))
           (_
            (loop rest (cdr out)))))
        ((component . rest)
         (loop rest (cons component out)))
        (()
         (string-join (reverse out) "/")))))

  (cond ((or (uri-scheme ref)
             (uri-host   ref))
         (build-uri (or (uri-scheme ref)
                        (uri-scheme base))
                    #:userinfo (uri-userinfo ref)
                    #:host     (uri-host     ref)
                    #:port     (uri-port     ref)
                    #:path     (remove-dot-segments (uri-path ref))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))
        ((string-null? (uri-path ref))
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments (uri-path base))
                    #:query    (or (uri-query ref)
                                   (uri-query base))
                    #:fragment (uri-fragment ref)))
        (else
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments
                                (if (string-prefix? "/" (uri-path ref))
                                    (uri-path ref)
                                    (merge-paths (uri-path base)
                                                 (uri-path ref))))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))))

(define (http-fetch uri)
  "Fetch data from URI and write it to FILE.  Return FILE on success."

  (define post-2.0.7?
    (or (> (string->number (major-version)) 2)
        (> (string->number (minor-version)) 0)
        (> (string->number (micro-version)) 7)
        (string>? (version) "2.0.7")))

  (define headers
    '(;; Some web sites, such as http://dist.schmorp.de, would block you if
      ;; there's no 'User-Agent' header, presumably on the assumption that
      ;; you're a spammer.  So work around that.
      (User-Agent . "GNU Guile")

      ;; Some servers, such as https://alioth.debian.org, return "406 Not
      ;; Acceptable" when not explicitly told that everything is accepted.
      (Accept . "*/*")))

  (let*-values (((connection)
                 (open-connection-for-uri uri))
                ((resp bv-or-port)
                 ;; XXX: `http-get*' was introduced in 2.0.7, and replaced by
                 ;; #:streaming? in 2.0.8.  We know we're using it within the
                 ;; chroot, but `guix-download' might be using a different
                 ;; version.  So keep this compatibility hack for now.
                 (if post-2.0.7?
                     (http-get uri #:port connection #:decode-body? #f
                               #:streaming? #t
                               #:headers headers)
                     (if (module-defined? (resolve-interface '(web client))
                                          'http-get*)
                         (http-get* uri #:port connection #:decode-body? #f
                                    #:headers headers)
                         (http-get uri #:port connection #:decode-body? #f
                                   #:extra-headers headers))))
                ((code)
                 (response-code resp))
                ((size)
                 (response-content-length resp)))
    (case code
      ((200)                                      ; OK
       (begin
         (call-with-output-file file
           (lambda (p)
             (if (port? bv-or-port)
                 (begin
                   (dump-port bv-or-port p
                              #:buffer-size %http-receive-buffer-size
                              #:progress (progress-proc (uri-abbreviation uri)
                                                        size))
                   (newline))
                 (put-bytevector p bv-or-port))))
         file))
      ((301                                       ; moved permanently
        302)                                      ; found (redirection)
       (let ((uri (resolve-uri-reference (response-location resp) uri)))
         (format #t "following redirection to `~a'...~%"
                 (uri->string uri))
         (close connection)
         (http-fetch uri file)))
      (else
       (error "download failed" (uri->string uri)
              code (response-reason-phrase resp))))))

(use-modules (ice-9 rdelim))

(receive (response body) (http-fetch "https://hacker-news.firebaseio.com/v0/maxitem.json")
  (pk response (read-string body)))
