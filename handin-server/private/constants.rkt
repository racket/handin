#lang racket/base

(require "config.rkt"
         racket/file)

(provide (all-defined-out))
(define ATTEMPT-DIR "ATTEMPT")
(define (success-dir n) (format "SUCCESS-~a" n))

(define (make-success-dir-available n)
  (let ([name (success-dir n)])
    (when (directory-exists? name)
      (if (< n (get-conf 'max-upload-keep))
          (begin (make-success-dir-available (add1 n))
                 (rename-file-or-directory name (success-dir (add1 n))))
          (delete-directory/files name)))))

(define ATTEMPT-RE (regexp (format "^~a$" ATTEMPT-DIR)))
(define SUCCESS-RE (regexp (format "^~a$" (success-dir "[0-9]+"))))
(define SUCCESS-GOOD (map success-dir '(0 1)))


(define user-assignment-directory (make-parameter #f))
(define (get-user-assignment-directory) (user-assignment-directory))

(define assignment-name (make-parameter #f))
(define (get-assignment-name) (assignment-name))

(define current-submission-data (make-parameter #f))
(define (get-submit-on-error?)
  (equal? (a-ref (current-submission-data) 'submit-on-error "no") "yes"))


;; errors to the user: no need for a "foo: " prefix
(define (error* fmt . args)
  (error (apply format fmt args)))

(define (write+flush port . xs)
  (for ([x (in-list xs)]) (write x port) (newline port))
  (flush-output port))

(define-struct alist (name [l #:mutable]))
(define (a-set! alist key val)
  (let ([l (alist-l alist)])
    (cond [(assq key l) => (lambda (p) (set-box! (cdr p) val))]
          [else (set-alist-l! alist (cons (cons key (box val)) l))])))
(define (a-ref alist key . default)
  (cond [(assq key (alist-l alist)) => (lambda (x) (unbox (cdr x)))]
        [(pair? default) (car default)]
        [else (error (alist-name alist) "no value for `~s'" key)]))
