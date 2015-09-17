#lang racket/base

(require racket/file
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
