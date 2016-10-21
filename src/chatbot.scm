(define-module (chatbot))

(use-modules (ice-9 match))


(define (main . args)
  (match args
    (("show" "me" "the" app) ...) ;; calendar, search, bookmark, messaging, blog
    ;;  calendar
    ;; (("add" "an" "event") ...) ;; trigger an automata
    (("add" "an" "event" when "at" time) ...)
    (("show" "me" "the" "next" "events") ...)
    (("show" "me" "the" "events" "for" date) ...)
    ;; search
    (("search" "for" query ...) ...)
    (("summarize" url) ...)
    (("extract" "article" "from" url) ...)
    ;; bookmarks
    (("bookmark" url) ...)
    (("tag" url tags ...) ...)
    (("search" "bookmarks" "for" query ...) ...)
    (("what" "is" "tagged" query ...) ...)
    ;; blog
    (("create" "a" "post" "entitled" title ...) ...) ;; need smarter parsing
    (("edit" "post" title ...) ...) ;; need smarter parsing
    (("publish" "post" title ...) ...) ;; need smater parsing
    ;; messaging
    (("check" "inbox") ...)
    (("send" "to" who message ...) ...) ;; need smarter parsing
    (("show" "me" "the" "timeline") ...)
    (("publish" "message" message ...) ...)))
