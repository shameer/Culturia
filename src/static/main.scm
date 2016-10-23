;; scheme helpers

(define (pk . args)
  (apply console-log (cons ";;;" args))
  (car (reverse args)))

(define (acons a b alist)
  (cons (cons a b) alist))

(define (alist-delete alist key)
  (let loop ((alist alist)
             (out '()))
    (if (null? alist)
        out
        (if (equal? (caar alist) key)
            (loop (cdr alist) out)
            (loop (cdr alist) (cons (car alist) out))))))

(define (alist-set alist key value)
  (acons key value (alist-delete alist key)))

(define (alist-ref alist key)
  (let loop ((alist alist))
    (cond
     ((null? alist) #f)
     ((equal? (caar alist) key) (cdar alist))
     (else (loop (cdr alist))))))

(define (string-prefix? prefix string)
  (let ((l (string-length prefix)))
    (if (< (string-length string) l)
        #f
        (let ((other (substring string 0 l)))
          (equal? other prefix)))))

;; snabbdom bindings

(define %patch (js-eval "patch"))

(define (patch old new)
  (js-call %patch old new)
  new)

(define %h (js-eval "h"))

(define (events->js-obj events)
  (alist->js-obj (map (lambda (name+proc)
                        (cons (car name+proc) (js-closure (cdr name+proc))))
                      events)))

(define (h tag events children)
  (js-call %h tag events (list->js-array children)))

(define (attrs->js-obj attrs)
  (let loop ((attrs attrs)
             (on '())
             (out '()))
    (if (null? attrs)
        (if (alist-ref out "key")
            (alist->js-obj `(("on" . ,(alist->js-obj on))
                             ("attrs" . ,(alist->js-obj out))
                             ("key" . ,(alist-ref out "key"))))
            (alist->js-obj `(("on" . ,(alist->js-obj on))
                             ("attrs" . ,(alist->js-obj out)))))
        (let ((name (symbol->string (caar attrs)))
              (value (cdar attrs)))
          (if (string-prefix? "on-" name)
              (loop (cdr attrs)
                    (cons (cons (substring name 3 (string-length name))
                                (js-closure value))
                          on)
                    out)
              (loop (cdr attrs)
                    on
                    (cons (cons name value) out)))))))

(define (sxml->h element)
  (if (not (list? element))
      element
      (let ((tag (symbol->string (car element))))
        (let ((attrs (cadr element)))
          (if (and (list? attrs) (eq? (car attrs) '@))
              (h (string-append tag "#") (attrs->js-obj (cdr attrs)) (map sxml->h (cddr element)))
              (h (string-append tag "#") (alist->js-obj '()) (map sxml->h (cdr element))))))))

;; FIXME: use biwascheme bindings

(define (event-target event)
  (js-ref event "target"))

(define (event-target-value event)
  (js-ref (event-target event) "value"))

(define (event-target-checked event)
  (js-ref (event-target event) "checked"))

(define (event-key event)
  (js-ref event "key"))

(define (event-prevent-default event)
  (js-invoke event "preventDefault"))

(define %json-parse (js-eval "JSON.parse"))

(define (json-parse string)
  (js-obj->alist (%json-parse string)))

(define (http-post* path alist)
  (json-parse (http-post path alist)))

;; FIXME: workaround the fact that ($ "#app") doesn't work
(define body (car (js-array->list ($ "body"))))
(define main (element-new '(div "Loading...")))
(element-append-child! body main)

;; framework-ish stuff

(define (render)
  (set! main (patch main (sxml->h (view *state*))))
  (js-eval "window.scrollTo(0,document.body.scrollHeight)"))

(define (make-action proc)
  (lambda args
    (set! *state* (apply (proc *state*) args))
    (render)))

;; app

(define *state* `())


(define (widget:input/readonly value)
  `(input (@ (type . "text")
             (readonly . #t)
             (value . ,value))))

(define (widget:input name)
  (define (on-keypress state)
    (lambda (event)
      (let ((key (event-key event)))
        (if (equal? key "Enter")
            (let ((value (event-target-value event)))
              (let ((response (http-post* "/api" `(("query" . ,value)))))
                (acons value (pk 'rez (js-obj->alist (alist-ref response "answer"))) state)))
            state))))

  `(input (@ (type . "text")
             (autofocus . #t)
             (on-keypress . ,(make-action on-keypress)))))

(define (view:hit hit)
  (let ((hit (js-obj->alist hit)))
    `(a (@ (class . "box")
           (href . ,(alist-ref hit "url")))
        (p (@ (class . "title")) ,(alist-ref hit "title"))
        (p (@ (class . "snippet")) ,(alist-ref hit "snippet"))
        (p (@ (class . "url")) ,(alist-ref hit "url")))))

(define (view:answer answer)
  (cond
   ((equal? (caar answer) "text")
    `(div (@ (class . "box"))
          (p ,(alist-ref answer "text"))))
   ((equal? (caar answer) "hits")
    `(div ,@(map view:hit (js-array->list (cdar answer)))))))

(define (view state)
  (pk 'state state)
  `(div (@ (id . "container"))
        (div (@ (id . "header")) (h1 "culturia"))
        ,@(map (lambda (pair)
                 `(div
                   (div (@ (class . "box input"))
                        ,(widget:input/readonly (car pair)))
                   ,(view:answer (cdr pair))))
               (reverse state))
        (div (@ (class . "box input"))
             ,(widget:input 'query))))

(render)
