#lang racket/base

(require racket/list
         racket/path
         racket/file
         racket/date
         net/uri-codec
         web-server/servlet
         web-server/compat/0/coerce
         web-server/compat/0/http/response-structs
         handin-server/private/md5
         handin-server/private/logger
         handin-server/private/config
         handin-server/private/hooker
         "run-servlet.rkt")

;; Looks up key in alist, returns #f if not found.
(define (aget alist key)
  (cond [(assq key alist) => cdr] [else #f]))

;; Remove spaces before and after s.
(define (clean-str s)
  (regexp-replace #rx" +$" (regexp-replace #rx"^ +" s "") ""))

;; Construct page title.
(define (make-page title . body)
  `(html (head (title ,title))
         (body ([bgcolor "white"]) (h1 ((align "center")) ,title) ,@body)))

;; Acess user data for a user.
(define get-user-data
  (let ([users-file (build-path server-dir "users.rktd")])
    (unless (file-exists? users-file)
      (log-line "WARNING: users file missing on startup: ~a" users-file))
    (lambda (user)
      (and user (get-preference (string->symbol user) (lambda () #f) 'timestamp
                                users-file)))))

;; Make path relative to server directory.
(define (relativize-path p)
  (path->string (find-relative-path (normalize-path server-dir) p)))

;; Construct an URL for downloading files.
;;
;; k should eventually call handle-status-request.
(define (make-k k tag #:mode [mode "download"])
  (let ([sep (if (regexp-match? #rx"^[^#]*[?]" k) "&" "?")])
    (format "~a~atag=~a~amode=~a" 
            k 
            sep
            (uri-encode tag)
            ";"
            (uri-encode mode))))

;; Find the directory with files look-for handed in for hi.
;;
;; `look-for' can be a username as a string (will find "bar+foo" for "foo"), or
;; a regexp that should match the whole directory name (used with "^solution"
;; below)
(define (find-handin-entry hi look-for)
  (let ([dir (assignment<->dir hi)])
    (and (directory-exists? dir)
         (ormap
          (lambda (d)
            (let ([d (path->string d)])
              (and (cond [(string? look-for)
                          (member look-for (regexp-split #rx" *[+] *" d))]
                         [(regexp? look-for) (regexp-match? look-for d)]
                         [else (error 'find-handin-entry
                                      "internal error: ~e" look-for)])
                   (build-path dir d))))
          (directory-list dir)))))

;; Display links to all files user handed in for hi
;; and/or links to upload such files now.
(define (handin-link k user hi upload-suffixes)
  (let* ([dir (find-handin-entry hi user)]
         [image (and dir (build-path dir "handin.png"))]
         [l (and dir (with-handlers ([exn:fail? (lambda (x) null)])
                       (parameterize ([current-directory dir])
                         (sort (filter (lambda (f)
                                         (and (not (equal? f "grade"))
                                              (not (equal? f "handin.png"))
                                              (file-exists? f)))
                                       (map path->string (directory-list)))
                               string<?))))])
    (append
     (if (pair? l)
         (cdr (append-map
               (lambda (f)
                 (let ([hi (build-path dir f)])
                   `((br)
                     (a ([href ,(make-k k (relativize-path hi))]) ,f)
                     " ("
                     ,(date->string
                       (seconds->date (file-or-directory-modify-seconds hi))
                       #t)
                     ")")))
               l))
         (list (format "No handins accepted so far for user ~s, assignment ~s"
                       user hi)))
     (if (and image (file-exists? image))
         (let ([image-k (make-k k (relativize-path image))])
           (list `(br)
                 `(a ([href ,image-k])
                     (img ([src ,image-k])))))
         null)
     (if upload-suffixes
         (let ([dir (or dir 
                        (build-path (assignment<->dir hi) user))])
           (list '(br)
                 `(a ([href ,(make-k k (relativize-path dir) #:mode "upload")])
                     "Upload...")))
         null))))

;; ???
(define (solution-link k hi)
  (let ([soln (and (member (assignment<->dir hi) (get-conf 'inactive-dirs))
                   (find-handin-entry hi #rx"^solution"))]
        [none `((i "---"))])
    (cond [(not soln) none]
          [(file-exists? soln)
           `((a ((href ,(make-k k (relativize-path soln)))) "Solution"))]
          [(directory-exists? soln)
           (parameterize ([current-directory soln])
             (let ([files (sort (map path->string
                                     (filter file-exists? (directory-list)))
                                string<?)])
               (if (null? files)
                 none
                 (apply append
                        (map (lambda (f)
                               `((a ([href ,(make-k k (relativize-path
                                                       (build-path soln f)))])
                                    (tt ,f))
                                 (br)))
                             files)))))]
          [else none])))

;; Load grade file for handin hi of user, or "--" by default.
(define (handin-grade user hi)
  (let* ([dir (find-handin-entry hi user)]
         [grade (and dir
                     (let ([filename (build-path dir "grade")])
                       (and (file-exists? filename)
                            (with-input-from-file filename
                              (lambda ()
                                (read-string (file-size filename)))))))])
    (or grade "--")))

;; Display the status of one user and one handin.
(define (one-status-page user for-handin)
  (let* ([next (send/suspend
                (lambda (k)
                  (make-page (format "User: ~a, Handin: ~a" user for-handin)
                    `(p ,@(handin-link k user for-handin #f))
                    `(p "Grade: " ,(handin-grade user for-handin))
                    `(p ,@(solution-link k for-handin))
                    `(p (a ([href ,(make-k k "allofthem")])
                           ,(format "All handins for ~a" user))))))])
    (handle-status-request user next null)))

;; Display a left-aligned cell in a handin table
(define (handin-table-cell  . texts)
  `(td ([bgcolor "white"]) ,@texts))

;; Display a right-aligned cell in a handin table.
(define (handin-table-rcell . texts)
  `(td ([bgcolor "white"] [align "right"]) ,@texts))

;; Display an header cell in a handin table.
(define (handin-table-header . texts)
  `(td ([bgcolor "#f0f0f0"]) (big (strong ,@texts))))

;; Displays a row in a table of handins.
(define (((handin-table-row user) k active? upload-suffixes) dir)
  (let ([hi (assignment<->dir dir)])
    `(tr ([valign "top"])
       ,(apply handin-table-header hi (if active? `((br) (small (small "[active]"))) '()))
       ,(apply handin-table-cell (handin-link k user hi upload-suffixes))
       ,(handin-table-rcell (handin-grade user hi)))))

;; Display the status of one user and all handins.
(define (all-status-page user)
  (define row (handin-table-row user))
  (define upload-suffixes (get-conf 'allow-web-upload))
  (let* ([next
          (send/suspend
           (lambda (k)
             (make-page
              (format "All Handins for ~a" user)
              `(table ([bgcolor "#ddddff"] [cellpadding "6"] [align "center"])
                 (tr () ,@(map handin-table-header '(nbsp "Files" "Grade")))
                 ,@(append (map (row k #t upload-suffixes) (get-conf 'active-dirs))
                           (map (row k #f #f) (get-conf 'inactive-dirs)))))))])
    (handle-status-request user next upload-suffixes)))

;; Handle file uploading and downloading.
;;
;; This function cooperates with make-k above.
(define (handle-status-request user next upload-suffixes)
  (let* ([mode (aget (request-bindings next) 'mode)]
         [tag (aget (request-bindings next) 'tag)])
    (cond
     [(string=? mode "download")
      (download user tag)]
     [(string=? mode "upload")
      (upload user tag upload-suffixes)]
     [else
      (error 'status "unknown mode: ~s" mode)])))

;; ???
(define (check path elts allow-active? allow-inactive?)
  (let loop ([path path] [elts (reverse elts)])
    (let*-values ([(base name dir?) (split-path path)]
                  [(name) (path->string name)]
                  [(check) (and (pair? elts) (car elts))])
      (if (null? elts)
          ;; must be rooted in a submission directory (why build-path instead
          ;; of using `path'? -- because path will have a trailing slash)
          (member (build-path base name)
                  (cond
                   [(and allow-active? allow-inactive?) (get-conf 'all-dirs)]
                   [allow-inactive? (get-conf 'inactive-dirs)]
                   [allow-active? (get-conf 'active-dirs)]
                   [else null]))
          (and (cond [(eq? '* check) #t]
                     [(regexp? check) (regexp-match? check name)]
                     [(string? check)
                      (or (equal? name check)
                          (member check (regexp-split #rx" *[+] *" name)))]
                     [else #f])
               (loop base (cdr elts)))))))

;; Handle downloading of files.
(define (download who tag)
  (define file (build-path server-dir tag))
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (log-line "Status exception: ~a" (exn-message exn))
                     (make-page "Error" "Illegal file access"))])
    ;; Make sure the user is allowed to read the requested file:
    (or (check file `(,who *) #t #t)
        (check file `(#rx"^solution") #f #t)
        (check file `(#rx"^solution" *) #f #t)
        (error 'download "bad file access for ~s: ~a" who file))
    (log-line "Status file-get: ~s ~a" who file)
    (hook 'status-file-get `([username ,(string->symbol who)] [file ,file]))
    ;; Return the downloaded file
    (let* ([data (file->bytes file)]
           [html? (regexp-match? #rx"[.]html?$" (string-foldcase tag))]
           [wxme? (regexp-match?
                   #rx#"^(?:#reader[(]lib\"read.(?:ss|rkt)\"\"wxme\"[)])?WXME"
                   data)])
      (make-response/full 200 #"Okay" (current-seconds)
        (cond [html? #"text/html"]
              [wxme? #"application/data"]
              [else  #"text/plain"])
        (list
         (make-header #"Content-Length"
                      (string->bytes/latin-1
                       (number->string (bytes-length data))))
         (make-header #"Content-Disposition"
                      (string->bytes/utf-8
                       (format "~a; filename=~s"
                               (if wxme? "attachment" "inline")
                               (let-values ([(base name dir?) (split-path file)])
                                 (path->string name))))))
        (list data)))))

;; Handle uploading of files.
(define (upload who tag suffixes)
  (define next
    (send/suspend
     (lambda (k)
       (make-page
        "Handin Upload"
        `(form ([action ,k] [method "post"] [enctype "multipart/form-data"])
               (table ([align "center"])
                      (tr (td "File:")
                          (td (input ([type "file"] [name "file"]))))
                      (tr (td ([colspan "2"] [align "center"])
                              (input ([type "submit"] [name "post"]
                                      [value "Upload"]))))))
        `(p "The uploaded file will replace any existing file with the same name.")
        `(p "Allowed file extensions:" 
            ,@(for/list ([s (in-list suffixes)]
                         [n (in-naturals)])
                `(span " " (tt ,(bytes->string/utf-8 s))))
            ". "
            "If the uploaded file has no extension or a different extension, " 
            (tt ,(bytes->string/utf-8 (first suffixes))) " is added automatically.")))))
  (let ([fb (for/first ([b (in-list (request-bindings/raw next))]
                        #:when (binding:file? b))
                       b)])
    (if (and fb
             (not (equal? #"" (binding:file-filename fb))))
        (let* ([fn (binding:file-filename fb)]
               [base-fn (if (for/or ([suffix (in-list suffixes)])
                              (regexp-match? (bytes-append (regexp-quote suffix) #"$") fn))
                            (bytes->path fn)
                            (path-add-suffix (bytes->path fn)
                                             (if (null? suffixes)
                                                 #".txt"
                                                 (car suffixes))))]
               [hw-dir (build-path server-dir tag)]
               [fn (build-path hw-dir (file-name-from-path base-fn))])
          (unless (check fn `(,who *) #t #f)
            (error 'download "bad upload access for ~s: ~a" who fn))
          (make-directory* hw-dir)
          (with-output-to-file 
              fn
              #:exists 'truncate/replace
              (lambda () (display (binding:file-content fb))))
          (all-status-page who))
        (error "no file provided"))))

;; Dispatch directly after login.
(define (status-page user for-handin)
  (log-line "Status access: ~s" user)
  (hook 'status-login `([username ,(string->symbol user)]))
  (if for-handin
    (one-status-page user for-handin)
    (all-status-page user)))

;; Display login.
(define (login-page for-handin errmsg)
  (let* ([request
          (send/suspend
           (lambda (k)
             (make-page
              "Handin Status Login"
              `(form ([action ,k] [method "post"])
                 (table ([align "center"])
                   (tr (td ([colspan "2"] [align "center"])
                           (font ([color "red"]) ,(or errmsg 'nbsp))))
                   (tr (td "Username")
                       (td (input ([type "text"] [name "user"] [size "20"]
                                   [value ""]))))
                   (tr (td nbsp))
                   (tr (td "Password")
                       (td (input ([type "password"] [name "passwd"]
                                   [size "20"] [value ""]))))
                   (tr (td ([colspan "2"] [align "center"])
                           (input ([type "submit"] [name "post"]
                                   [value "Login"])))))))))]
         [bindings  (request-bindings request)]
         [user      (aget bindings 'user)]
         [passwd    (aget bindings 'passwd)]
         [user      (and user (clean-str user))]
         [user      (and user (if (get-conf 'username-case-sensitive)
                                user (string-foldcase user)))]
         [user-data (get-user-data user)])
    (redirect/get)
    (cond [(and user-data
                (string? passwd)
                (let ([pw (md5 passwd)])
                  (or (equal? pw (car user-data))
                      (equal? pw (get-conf 'master-password)))))
           (status-page user for-handin)]
          [else (login-page for-handin "Bad username or password")])))

;; Set up session counter.
(define web-counter
  (let ([sema (make-semaphore 1)] [count 0])
    (lambda ()
      (dynamic-wind
        (lambda () (semaphore-wait sema))
        (lambda () (set! count (add1 count)) (format "w~a" count))
        (lambda () (semaphore-post sema))))))

;; Fetch current error-print-context-length.
;;
;; The error-print-context-length parameter is set to 0 in run
;; below, but reset to default-context-length in dispatcher.
(define default-context-length (error-print-context-length))

;; Entry point for one connection.
(define (dispatcher request)
  (error-print-context-length default-context-length)
  (parameterize ([current-session (web-counter)])
    (login-page (aget (request-bindings request) 'handin) #f)))

;; Entry point for the whole HTTPS server.
(provide run)
(define (run)
  (if (get-conf 'use-https)
    (begin0 (parameterize ([error-print-context-length 0])
              (run-servlet
               dispatcher
               #:log-file (get-conf 'web-log-file)))
      (log-line "*** embedded web server started"))
    ;; simple "server" so it's known that there is no server
    (lambda (msg . args)
      (when (eq? 'connect msg)
        (for-each (lambda (x) (display x (cadr args)))
                  '(#"HTTP/1.0 200 OK\r\n"
                    #"Content-Type: text/html\r\n"
                    #"\r\n"
                    #"<html><body><h1>"
                    #"Please use the handin plugin"
                    #"</h1></body></html>"))
        (close-input-port (car args))
        (close-output-port (cadr args))
        (semaphore-post (caddr args))))))
