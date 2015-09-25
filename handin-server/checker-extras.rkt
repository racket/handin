#lang racket
(provide (all-defined-out))

;; Checker utilities to save state across runs of the checker.

; The checker is run in a per-submission directory, so we go up to access a state shared across submissions.
; This is inside $handin/$user.
(define status-path (string->path "../status"))

(define (write-to value path)
  (call-with-output-file path
    (lambda (out) (write value out))
    #:exists 'truncate))
(define (read-from path)
  (call-with-input-file path (lambda (in) (read in))))

(define (read-status-internal) (read-from status-path))
(define (write-status value) (write-to value status-path))

(define (read-status)
  (if (file-exists? status-path) (read-status-internal) 'init))
