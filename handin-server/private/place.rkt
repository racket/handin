#lang racket/base

(require racket/place
         racket/match
         racket/file
         racket/format
         racket/place
         racket/port
         "logger.rkt"
         "reloadable.rkt"
         (submod "config.rkt" pure)
         "constants.rkt")

(define (checker-place-loop ch checker-path server-dir assignment ATTEMPT-DIR dirname)
  (define (run-checker-place pre checker post)
    (match (place-channel-get ch)
      [(list 'pre users s)
       (when pre
         (let ([dir (current-directory)])
           (with-handlers
             ([void (lambda (e)
                      (parameterize ([current-directory dir])
                        (unless (ormap (lambda (d)
                                         (and (directory-exists? d)
                                            (regexp-match
                                             SUCCESS-RE
                                             (path->string d))))
                                       (directory-list))
                          (parameterize ([current-directory ".."])
                            (when (directory-exists? dirname)
                              (delete-directory/files dirname)))))
                      (raise e))])
             (parameterize ([current-directory ATTEMPT-DIR])
               (pre users s)))))]
      [(list 'checker users s)
       (define result
         (parameterize ([current-directory ATTEMPT-DIR])
           (checker users s)))
       (if (string? result)
           (place-channel-put ch result)
           (place-channel-put ch "handin"))]
      [(list 'post users s dir)
       (parameterize ([current-directory dir])
         (and post (post users s dir)))]
      ['stop 'stop]))


  (define checker*
    (parameterize ([current-directory server-dir])
      (auto-reload-value
       `(file ,(path->string checker-path))
       'checker)))
  (parameterize ([user-assignment-directory (path->complete-path (build-path 'same))]
                 [assignment-name assignment]
                 [current-directory dirname])
    (define-values (pre checker post)            
      (cond [(not checker*) (values #f #f #f)]
            [(procedure? checker*) (values #f checker* #f)]
            [(and (list? checker*) (= 3 (length checker*)))
             (apply values checker*)]
            [else (place-channel-put (~e checker*))
                  (error* "bad checker value: ~e" checker*)]))
    (place-channel-put ch (list (and pre 'pre)
                                (and checker 'checker)
                                (and post 'post)))
    (let loop ()
      (define v (with-handlers ([void (lambda (e)
                                        (log-line "error was raised: ~s" e)
                                        (place-channel-put ch (cons 'exn (exn-message e)))
                                        'stop)])
                  (run-checker-place pre checker post)))
      
      (unless (eq? v 'stop) (loop)))))


(provide start-place)
(define (start-place checker* server-dir assignment ATTEMPT-DIR dirname)
  (define d (current-directory))
  (place/context p
    (parameterize ([current-directory d])
      (with-handlers ([void (lambda (e)
                              (log-line "error was raised: ~s" e)
                              (place-channel-put p (cons 'exn (exn-message e)))
                              'stop)])
        (checker-place-loop p checker* server-dir assignment ATTEMPT-DIR dirname)))))
