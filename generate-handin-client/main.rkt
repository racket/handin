#lang racket

(require pkg)
(require setup/getinfo)
(require srfi/54)

; print something in a style that works for #lang setup/infotab
(define (print-info v)
  (cond
    [(string? v)
     (display v)
     (newline)]
    [else
     (print v (current-output-port) 1)
     (newline)]))

; copy a file
(define (copy name source target)
  (cond
    [(file-exists? source)
     (printf "copying ~a from ~a to ~a~n" name source target)
     (copy-file source target #t)]
    [else
     (raise-user-error 'copy "missing ~a (in file ~a)" name source)]))

; Where to copy the handin-client from
(define source-dir
  (collection-path "handin-client"))

; Where to generate the handin-client in
(define target-dir
  (current-directory))

; Where to find the configuration
(define conf-dir
  (current-directory))

; Configuration file
(define conf-file
  (build-path conf-dir "config.rktd"))

; Access configuration settings
(define get-conf
  (curry hash-ref
         (for/hasheq ([entry (file->value conf-file)])
           (apply values entry))))

; Name of the generated client
(define client-name
  (get-conf 'client-name))

; Name of the generated package
(define package-name
  client-name)

; Name of the generated collection
(define collection-name
  client-name)

; The generated package directory
(define package-dir
  (build-path target-dir package-name))

; The generated collection directory
(define collection-dir
  (build-path package-dir collection-name))

; Name of the course
(define course-name
  (get-conf 'course-name client-name))

; Name of the server
(define server-name
  (get-conf 'server-name "localhost"))

; Port of the server
(define port-number
  (get-conf 'port-number 7979))

; URL of the course's homepage, if any
(define web-address
  (get-conf 'web-address #f))

; Enable auto-update? & Auto-update relative URL.
; Depends on web-address being defined.
;(define auto-update-address #f)
(define auto-update-address "racket")

; Name of the course's homepage
(define web-menu-name
  (get-conf 'web-menu-name (string-append course-name " Homepage")))

; Fallback icon
(define icon-fallback
  (get-conf 'icon "icon.png"))

; Icon for splash screen, about screen etc. (32px high)
(define icon-splash
  (get-conf 'icon-splash icon-fallback))

; Icon for toolbar button (16px high)
(define icon-button
  (get-conf 'icon-button icon-fallback))

; Icon for status server (arbitrary size)
(define icon-server
  (get-conf 'icon-server icon-fallback))

; Create package directory
(printf "creating package directory ~a~n" package-dir)
(make-directory package-dir)

; Copy handin-client collection
(let ([source source-dir]
      [target collection-dir])
  (printf "copying handin-client from ~a to ~a~n" source target)
  (copy-directory/files source target))

; Create package-level info.rkt
(let ([path (build-path package-dir "info.rkt")])
  (printf "writing package info to ~a~n" path)
  (with-output-to-file path
    (lambda ()
      (print-info "#lang setup/infotab")
      (print-info '(define collection 'multi)))))

; Create collection-level info.rkt
(let ([path (build-path collection-dir "info.rkt")])
  (printf "writing collection info to ~a~n" path)
  (with-output-to-file path
    #:exists 'truncate/replace
    (lambda ()
      (print-info "#lang setup/infotab")

      ; course name
      (print-info `(define name ,course-name))

      ; user interface
      (print-info `(define drracket-tools `("client-gui.rkt")))
      (print-info `(define drracket-tool-names `(,name)))
      (print-info `(define drracket-tool-icons `("icon-splash.png")))

      ; server:port
      (print-info `(define server:port ,(format "~a:~a" server-name port-number)))

      ; homepage
      (when web-address
        (print-info `(define web-menu-name ,web-menu-name))
        (print-info `(define web-address ,web-address))

        (when auto-update-address
          ;; Auto-updater section (see handin-server/doc.txt for details)
          (print-info `(define enable-auto-update #t))
          (print-info `(define version-filename ,(string-append auto-update-address "/" client-name ".version")))
          (print-info `(define package-filename ,(string-append auto-update-address "/" client-name ".plt")))))

      ; dependencies
      (print-info `(define requires '(("mred") ("openssl")))))))

; Format v, zero-padding it to the left to have at least n-digits digits in total.
(define (zero-pad n-digits v)
  (cat v n-digits #\0))

(let ([path (build-path collection-dir "version")]
      [now-date (seconds->date (current-seconds))])
  (printf "writing version info to ~a~n" path)
  (with-output-to-file path
    #:exists 'truncate/replace
    (lambda ()
      (print-info (format "~a~a~a~a~a"
                          (zero-pad 4 (date-year now-date))
                          (zero-pad 2 (date-month now-date))
                          (zero-pad 2 (date-day now-date))
                          (zero-pad 2 (date-hour now-date))
                          (zero-pad 2 (date-minute now-date)))))))


; Copy icon for splash screen to collection
(let* ([source (build-path conf-dir icon-splash)]
       [target (build-path collection-dir "icon-splash.png")])
  (when (file-exists? target)
    (delete-file target))
  (copy "icon for splash screen" source target))

; Copy icon for toolbar button to collection
(let* ([source (build-path conf-dir icon-button)]
       [target (build-path collection-dir "icon.png")])
  (when (file-exists? target)
    (delete-file target))
  (copy "icon for toolbar button" source target))

; Copy SSL certificate to collection
(let* ([file "server-cert.pem"]
       [source (build-path conf-dir file)]
       [target (build-path collection-dir file)])
  (copy "SSL certificate" source target))

; Delete compiled code
; (I would prefer to use #:source #t below, but it raises an exception)
(printf "cleaning package source~n")
(delete-directory/files (build-path collection-dir "compiled"))

; Create package
(pkg-create-command
   #:format 'zip
   #:from-dir #t
   #:dest target-dir
   package-dir)

; Delete configured package source
(printf "deleting package source~n")
(when (directory-exists? package-dir)
  (delete-directory/files package-dir))
