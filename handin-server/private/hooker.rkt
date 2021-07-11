#lang racket/base

(require "config.rkt" "logger.rkt" "reloadable.rkt")

(provide hook extra-dispatcher)

(define hook-file #f)
(define hook-proc #f)
(define (hook what alist)
  (let ([file (get-conf 'hook-file)])
    (when file
      (unless (equal? file hook-file)
        (set! hook-file file)
        (set! hook-proc (auto-reload-procedure `(file ,(path->string file))
                                               'hook)))
      (hook-proc what (current-session) alist))))

(define dispatcher-file #f)
(define dispatcher-proc #f)
(define ((extra-dispatcher otherwise) connection request)
  (let ([file (get-conf 'extra-dispatcher-file)])
    (cond [(not file) (otherwise)]
          [else (unless (equal? file dispatcher-file)
                  (set! dispatcher-file file)
                  (set! dispatcher-proc
                        (auto-reload-procedure `(file ,(path->string file))
                                               'dispatcher)))
                (dispatcher-proc connection request)])))
