#lang racket/base

(require racket/file
         json
         net/http-client
         net/uri-codec
         "logger.rkt"
         "config.rkt")

;; Acess user data for a user.
(provide get-user-data)
(define get-user-data
  (let ([users-file (build-path server-dir "users.rktd")])
    (unless (file-exists? users-file)
      (log-line "WARNING: users file missing on startup: ~a" users-file))
    (lambda (user)
      (and user (get-preference (string->symbol user) (lambda () #f) 'timestamp
                                users-file)))))

;; cache results of action for up to freq milliseconds
(define (cached freq action)
  (let ([cache #f]
        [last #f])
    (lambda ()
      (if (and last (< (- (current-inexact-milliseconds) last) freq))
        cache
        (let ([result (action)])
          (set! cache result)
          (set! last (current-inexact-milliseconds))
          result)))))

;; access discourse configuration
(define get-conf/discourse
  (let* ([read-discourse-config-file
          (lambda ()
            (let* ([file (get-conf 'discourse-config-file)]
                   [text (and file (file->string file))]
                   [result (and text (string->jsexpr text))])
              result))]
         [discourse-config
          (cached 2000.0 read-discourse-config-file)])
    (lambda (key)
      (let ([config (discourse-config)])
        (and config (hash-ref config key))))))

;; send request to discourse
(define (discourse path [post-data #f])
  (let ([api-username (get-conf/discourse 'api_username)]
        [api-key (get-conf/discourse 'api_key)])
    (and api-username api-key
      (let-values ([(status header port)
                    (http-sendrecv "forum-ps.informatik.uni-tuebingen.de"
                                   (format "~a?~a"
                                     path
                                     (alist->form-urlencoded `((api_key . ,api-key)
                                                               (api_username . ,api-username))))
                                   #:ssl? #t
                                   #:version "1.1"
                                   #:method (if post-data "POST" "GET")
                                   #:data post-data)])
        (log-line  "DISCOURSE ~a: ~a" path status)
        (define result (read-json port))
        (close-input-port port)
        result))))

;; fetch user database of discourse
(define get-user-data/discourse
  (let* ([fetch-data
          (lambda ()
            (for/hasheq ([user (hash-ref (discourse "/admin/course/dump.json") 'users)])
              (values (string->symbol (hash-ref user 'username)) user)))]
         [data (cached 2000.0 fetch-data)])
    (lambda (username)
      (hash-ref (data) username))))

;; authenticate username/password with discourse
(define (has-password/discourse? username password)
  (discourse "/admin/course/auth.json" (alist->form-urlencoded `((user . ,username) (password . ,password)))))

(define crypt
  (let ([c #f] [sema (make-semaphore 1)])
    ;; use only when needed so it doesn't blow up on non-unix platforms
    (lambda (passwd salt)
      (unless c (set! c (dynamic-require 'ffi/crypt 'crypt)))
      ;; crypt is not reentrant
      (call-with-semaphore sema
                           (lambda () (bytes->string/utf-8 (c passwd salt)))))))

(provide make-has-password?)
(define ((make-has-password? error*) raw md5 passwords)
  (define (good? passwd)
    (define (bad-password msg)
      (log-line "ERROR: ~a -- ~s" msg passwd)
      (error* "bad password in user database"))
    (cond [(string? passwd) (equal? md5 passwd)]
          [(and (list? passwd) (= 2 (length passwd))
                (symbol? (car passwd)) (string? (cadr passwd)))
           (case (car passwd)
             [(plaintext) (equal? raw (cadr passwd))]
             [(unix)
              (let ([salt (regexp-match #rx"^([$][^$]+[$][^$]+[$]|..)"
                                        (cadr passwd))])
                (unless salt (bad-password "badly formatted unix password"))
                (equal? (crypt raw (car salt)) (cadr passwd)))]
             [else (bad-password "bad password type in user database")])]
          [else (bad-password "bad password value in user database")]))
  (or (member md5 passwords) ; very cheap search first
      (ormap good? passwords)))
