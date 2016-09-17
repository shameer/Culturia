(use-modules ((htmlprag) #:select (html->sxml)))
(use-modules ((srfi srfi-1) #:select (fold)))
(use-modules (html))
(use-modules (ice-9 match))
(use-modules (ice-9 receive))
(use-modules (ice-9 regex))
(use-modules (path))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))
(use-modules (sxml simple))
(use-modules (sxml ssax))
(use-modules (uav))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))

(setlocale LC_ALL "")


(define (for-each-element-in-file filepath proc)
  (call-with-input-file filepath
    (lambda (port)
      ((ssax:make-parser
        NEW-LEVEL-SEED
        (lambda (elem-gi attributes namespaces expected-content seed)
          '())

        FINISH-ELEMENT
        (lambda (tag attributes namespaces parent-seed seed)
          (proc tag attributes))

        CHAR-DATA-HANDLER
        (lambda (string1 string2 seed)
          (if (string-null? string2) (cons string1 seed)
              (cons* string2 string1 seed)))

        DOCTYPE
        (lambda (port docname systemid internal-subset? seed)
          (values #f '() '() seed))

        UNDECL-ROOT
        (lambda (elem-gi seed)
          (values #f '() '() seed))

        PI
        ((*DEFAULT* .
                    (lambda (port pi-tag seed)
                      (cons
                       (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                       seed))))
        )
       port '()))))

(define (load work)
  (let ((db (path-join work "db")))
    (when (access? db F_OK)
      (rmtree db))
    (path-mkdir db)
    (format #t "* building database at ~s\n" db)
    (let ((cnx (uav-open* db)))
      (with-cnx cnx
        (for-each
         (lambda (filename)
           (let ((filepath (path-join "stackoverflow/dump" (string-append filename ".xml"))))
             (format #t "** loading ~s\n" filepath)
             (for-each-element-in-file filepath
               (lambda (tag row)
                 (when (eq? tag 'row)
                   (uav-add! (map (match-lambda ((key . value)
                                                 (cons (symbol-append (string->symbol filename) '/ key)
                                                       value)))
                                  row)))))))
         (list "Posts" "Comments" "PostLinks" "Users"))))))


(define (comment-ref uid)
  (let* ((comment (uav-ref* uid))
         (user (if (assoc-ref comment 'Comments/UserId)
                   (uav-ref* (car (uav-index-ref 'Users/Id (assoc-ref comment 'Comments/UserId))))
                   "")))
    (acons 'Comments/User user comment)))

(define (post-ref uid)
  (let* ((post (uav-ref* uid))
         (comments (uav-index-ref 'Comments/PostId (assoc-ref post 'Posts/Id)))
         (comments (map comment-ref comments))
         (comments (sort comments (lambda (a b) (string<? (assoc-ref a 'Comments/CreationDate)
                                                          (assoc-ref b 'Comments/CreationDate)))))
         (post (acons 'Posts/Comments comments post)))
    (let* ((user (car (uav-index-ref 'Users/Id (assoc-ref post 'Posts/OwnerUserId))))
           (post (acons 'Posts/User (uav-ref* user) post)))
      post)))

(define (question-ref uid)
  (let* ((question (post-ref uid))
         (answers (uav-index-ref 'Posts/ParentId (assoc-ref question 'Posts/Id)))
         (answers (map post-ref answers)))
    (acons 'Posts/Answers answers question)))

(define (alphanumeric? char)
  (member (char-downcase char) (string->list "abcdefghijklmnopqrstuvwxyz0123456789")))

(define (slugify string)
  (list->string
   (map (match-lambda ((? alphanumeric? char) (char-downcase char))
                      (_ #\-))
        (string->list string))))

(define (split-tags tags)
  (map (lambda (tag) (string-drop (string-take tag (1- (string-length tag))) 1))
       (map match:substring (list-matches "<[^>]+>" (or tags "")))))

(define (relateds question)
  (map (lambda (uid)
         (uav-ref* (car (uav-index-ref 'Posts/Id (assoc-ref (uav-ref* uid) 'PostLinks/RelatedPostId)))))
       (uav-index-ref 'PostLinks/PostId (assoc-ref question 'Posts/Id))))

(define (template:post post)
  `(div (@ (class "post"))
        (div (@ (class "main"))
             (div (@ (class "reputation"))
                  (div (@ (class "score"))
                       score: ,(assoc-ref post 'Posts/Score))
                  (div (@ (class "favorites"))
                       favorites: ,(or (assoc-ref post 'Posts/FavoriteCount) 0))
                  (div (@ (class "views"))
                       views: ,(or (assoc-ref post 'Posts/ViewCount) 0)))
             (div
              (div (@ (class "body"))
                  ,(cdr (html->sxml (assoc-ref post 'Posts/Body))))
              (div (@ (class "metadata"))
                   (ul (@ (class "tags"))
                       ,(map (lambda (name) `(li (a (@ (href ,(string-append "../tags/" name))) ,name)))
                             (split-tags (assoc-ref post 'Posts/Tags))))
                   (div (@ (class "user"))
                        (p ,(assoc-ref (assoc-ref post 'Posts/User) 'Users/DisplayName))))
              (div (@ (class "comments"))
                   ,(map (lambda (comment)
                           `(div (@ (class "comment"))
                                 (p
                                  (span ,(assoc-ref comment 'Comments/Score)) " "
                                  (span ,(assoc-ref comment 'Comments/Text)) " "
                                  (span ,(assoc-ref (assoc-ref comment 'Comments/User) 'Users/DisplayName)) " "
                                  (span ,(assoc-ref comment 'Comments/CreationDate))
                                 )))
                         (assoc-ref post 'Posts/Comments)))))))

(define (template:question question)
  `(html
    (head
     (meta (@ (charset "utf-8")))
     (meta (@ (url ,(assoc-ref question 'Posts/Id))))
     (title "stackoverflow - " ,(assoc-ref question 'Posts/Title)))
    (body (@ (class "question"))
          (div (@ (class "container"))
               (h1 "stackoverflow")
               (div (@ (class "container"))
                    (div (@ (class "question"))
                         (div 
                          (h2 ,(assoc-ref question 'Posts/Title))
                          ,(template:post question))
                         (div (@ (class "answers"))
                              (h2 "Answers")
                              ,(map template:post (assoc-ref question 'Posts/Answers))))
                    (div (@ (class "related"))
                         (h3 "Related questions")
                         (ul ,(map (lambda (related)
                                     `(li (a (@ (href ,(string-append (slugify (assoc-ref related 'Posts/Title))
                                                                      ".html")))
                                             ,(assoc-ref related 'Posts/Title))))
                                   (relateds question)))))))))
  
(define (render-question questions-directory question)
  (catch #true
    (lambda ()
      (let* ((filename (path-join questions-directory
                                  (string-append (slugify (assoc-ref question 'Posts/Title))
                                                 ".html"))))
        (format #t "** rendering ~s\n" filename)
        (call-with-output-file filename
          (lambda (port)
            (sxml->html (template:question question) port)))))
    (lambda _ (format #t "** failed"))))

(define (render work)
  (let* ((db (path-join work "db"))
         (cnx (uav-open* db)))
    (format #t "* using database at ~s\n" db)
    (let ((build (path-join work "build")))
      (when (access? build F_OK)
        (rmtree build))
      (path-mkdir build)
      (system (format #f "cp -r ~a ~a" (path-join "sotoki-static") (path-join build "static")))
      ;; render questions
      (let ((questions-directory (path-join build "questions")))
        (when (access? questions-directory F_OK)
          (rmtree questions-directory))
        (path-mkdir questions-directory)
        (format #t "* rendering questions in ~s\n" questions-directory)
        (let* ((uids (list->stream (uav-index-ref 'Posts/PostTypeId "1")))
               (questions (stream-map question-ref uids)))
          (stream-for-each (cut render-question questions-directory <>) questions))))))
    
(load "stackoverflow")
(render "stackoverflow")


;;;
;;; tests
;;;

(when (or (getenv "CHECK") (getenv "CHECK_SOTOKI"))
  (format #t "* Checking sotoki\n")
  (test-check "slugify"
    (slugify "abc *-def")
    "abc---def"))

;;; Local Variables:
;;; compile-command: "guile -L . sotoki.scm"
;;; End:
