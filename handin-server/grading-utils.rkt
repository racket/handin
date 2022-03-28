#lang racket

(require racket/date)
(require handin-server/checker)
(require handin-server/private/config)
(require web-server/private/timer)
(require (only-in handin-server/utils get-assignment-name))

(provide check-deadline
         check-max-submissions 
         
         update-submission-timestamp!
         get-submission-timestamp         
         
         set-test-max-score!
         score-add-penalty!
         
         add-report-line!         
         with-output-to-report         
         add-score-to-report!
         write-report
         
         @test
         @test/exn)

(define student-score (make-thread-cell #f))
(define test-max-score (make-thread-cell #f))
(define submission-timestamp (make-thread-cell #f))

;; set-test-max-score!: positive int -> void
(define (set-test-max-score! max-score)
  (let ([cur-student-score (thread-cell-ref student-score)]
        [cur-test-max-score (thread-cell-ref test-max-score)])
    (if cur-student-score
        (set-box! cur-student-score max-score)
        (thread-cell-set! student-score (box max-score)))
    (if cur-test-max-score
        (set-box! cur-test-max-score max-score)
        (thread-cell-set! test-max-score (box max-score)))))

(define (update-submission-timestamp!)
  (let ([cur-submission-timestamp (thread-cell-ref submission-timestamp)])
    (define ts (begin (date-display-format 'iso-8601)
                      (date->string (current-date) #t)))
    (if cur-submission-timestamp
        (set-box! cur-submission-timestamp ts)
        (thread-cell-set! submission-timestamp (box ts)))))

;; penalty must be a positive number
(define (score-add-penalty! penalty)
  (let ([cur-student-score (thread-cell-ref student-score)])
    (if cur-student-score
        (set-box! cur-student-score (max 0 (- (get-student-score) penalty)))
        (error "student-score should be initialized already"))))

(define (get-student-score) (unbox (thread-cell-ref student-score)))

(define (get-test-max-score) (unbox (thread-cell-ref test-max-score)))

(define (get-submission-timestamp)
  (let ([cur-submission-timestamp (thread-cell-ref submission-timestamp)])
    (if cur-submission-timestamp
        (unbox cur-submission-timestamp)
        #f)))  

(define (add-score-to-report!)
  (add-report-line! (format "Final Score: ~a out of ~a"
                            (get-student-score)
                            (get-test-max-score))))

(define report-lines (make-thread-cell #f))

(define (add-report-line! line)
  (let ([new (list line)]
        [cur (thread-cell-ref report-lines)])
    (if cur
        (set-box! cur (append (unbox cur) new))
        (thread-cell-set! report-lines (box new)))))

(define (get-report-delay-in-minutes) 
  (with-handlers ([exn? (thunk* 0)])
    (get-conf 'report-delay-in-minutes)))

(define-syntax (write-report stx)
  (define (id s) (datum->syntax stx s stx))
  (syntax-case stx ()
    [(write-report)
     (with-syntax ([users (id 'users)])
       #'(begin
           (define dir (build-path (current-directory) 'up))
           (define ts (get-submission-timestamp))
           (define (report->string users)
             (with-output-to-string
              (lambda ()
                (for-each (lambda (str) (printf "~a\n" str))
                          (cond [(thread-cell-ref report-lines) => unbox]
                                [else '()])))))  
           (define report-string (report->string users))
           (define report-delay (get-report-delay-in-minutes))
           (start-timer (start-timer-manager)
                        (* 60 report-delay)
                        (thunk
                         (with-output-to-file
                             (build-path dir 
                                         (format "~a-report-~a.txt"
                                                 (string-join users "+")                                        
                                                 ts))
                           #:exists 'replace
                           (thunk (display report-string)))))
           (message (string-append "Your submission has been accepted."
                                   (format " A report with timestamp ~a will be available after ~a minutes"
                                           ts
                                           report-delay))
                    '(ok))))]))

(define (get-deadline-seconds dir-name)
  (match (assoc dir-name (get-conf 'deadline))
    [(list _ (list year month day hours minutes seconds) max-late-days)
     (values (find-seconds seconds minutes hours day month year)
             (find-seconds seconds minutes hours (+ day max-late-days) month year))]
    [else (values #f #f)]))


(define (check-deadline)
  (define dir-name (get-assignment-name))
  (set-run-status "Checking submission against deadline")  
  (define-values (deadline deadline+late-days) (get-deadline-seconds dir-name))
  (define now (current-seconds))
  (when (and deadline (> now deadline))
    (cond
      [(> now deadline+late-days) (error "Your submission is rejected because it is past the deadline + extra days")]
      [else (message "Your submission is accepted but it is late, you will get a discount in your grade" '(ok))])))

(define (success-dir n) (format "SUCCESS-~a" n))
(define SUCCESS-RE (regexp (format "^~a$" (success-dir "[0-9]+"))))

(define (successful-submissions)
  ;; Find the number of SUCCESS-? dirs in the user directory
  (let ([dirlist (map path->string (directory-list ".."))])      
    (length (filter (lambda (d)
                      (and (directory-exists? (string->path (format "../~a" d)))
                           (regexp-match SUCCESS-RE d)))
                    dirlist))))

(define (check-max-submissions)
  (define dir-name (get-assignment-name))
  (set-run-status "Checking against maximum number of submissions")
  (let ([max-submissions-conf (assoc dir-name (get-conf 'max-submissions))])      
    ;; setting max-submissions <= 0 means unlimited submissions
    (when max-submissions-conf
      (let ([max-submissions (second max-submissions-conf)]
            [used-submissions (successful-submissions)])
        (when (> max-submissions 0)
          (if (>= used-submissions max-submissions)
              (error (format "You already used your ~a submissions" max-submissions))
              (case (message (string-append "Are you sure you want to proceed? "
                                            (format "If this submission is successful you will have ~a out of ~a"
                                                    (- max-submissions used-submissions 1)
                                                    max-submissions)
                                            "submissions left.")
                             '(yes-no))
                ((no) (error "Submission aborted by user")))))))))

(define-syntax with-output-to-report
  (syntax-rules ()
    [(_ expr ...)     
     (let ([output-port (open-output-string)])
       (parameterize ([current-output-port output-port])
         (call-with-values
          (λ () expr ...)
          (λ args
            (close-output-port output-port)
            (add-report-line! (get-output-string output-port))
            (apply values args)))))]))

(define-syntax @test
  (syntax-rules ()
    [(_ test-desc error-desc expr result penalty)
     (@test test-desc error-desc expr result equal? penalty)]
    
    [(_ test-desc error-desc expr result equal? penalty)
     (with-handlers
         ([exn? (λ (exn) 
                  (with-output-to-report
                   (displayln (exn-message exn))
                   (score-add-penalty! penalty)
                   (newline)
                   (displayln (format "Penalty: ~a" penalty))))])
       (with-output-to-report
        (display (format "Test: ~a" test-desc)))
       (!test expr result equal?))]))

(define-syntax @test/exn
  (syntax-rules ()
    [(_ test-desc expr penalty)
     (with-handlers
         ([exn? (λ (exn) 
                  (with-output-to-report
                   (display (exn-message exn))
                   (score-add-penalty! penalty)
                   (display (format "Penalty: ~a" penalty))))])
       (with-output-to-report
        (display (format "Test: ~a" test-desc)))
       (!test/exn expr))]))

