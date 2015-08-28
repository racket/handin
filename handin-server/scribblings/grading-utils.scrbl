#lang scribble/doc
@(require "common.rkt"
           (for-label handin-server/grading-utils))

@title[#:tag "grading-utils"]{Grading Utilities}

@defmodule[handin-server/grading-utils]

This module provides utilities that can serve as the basis for an automated
grading system. In particular, it provides:

@itemlist[

@item{A mechanism to establish deadlines with or without allowing late
submissions. The idea is to avoid having someone stop the server at midnight in
order to stop accepting submissions.}

@item{A mechanism to establish a maximum number of submissions. The idea is
that students can get limited feedback before doing the definitive submission.}

@item{A mechanism to keep track of the user's score, and where failed tests add
a penalty to that score.}

@item{A mechanism to create a report (a text file) of the evaluation of the submission,
instead of reporting errors immediately when they are found. Moreover, it is
possible to delay the publication of this report, in order to avoid potential
abuses from the students.}

]

The following example illustrates a checker module using this infrastructure:

@racketblock[

(module checker handin-server/checker 
  
  (require handin-server/grading-utils)

  (code:comment "Checks that submission is on time and that the user has submissions left")
  (pre:
   (check-deadline)
   (check-max-submissions))
  
  (code:comment "Ends the report by adding the score and writes it in the user directory")
  (code:comment "This way, students can see their reports from the web interface.")
  (post:
   (add-score-to-report!)
   (write-report))
  
  (check:   
   (code:comment "Get timestamp of the submission and add it to header and report")
   (update-submission-timestamp!)
   (add-header-line! (get-submission-timestamp))
   (add-report-line! (get-submission-timestamp))
   
   (code:comment "Acceptance tests: reject the submission if any of these tests fail")
   (!test (foo 1) 3)
   (code:comment "...")
   
   (code:comment "Grading")
   
   (code:comment "Initialize max score")
   (set-test-max-score! 100)
   
   (code:comment "Failure discounts 25 points")
   (@test "Sample case 1"
          "Error using even? predicate"
          (bar '(1 2 3 4) even?)
          '((2 4)(1 3))
          25)
   ))
]

@defproc[(check-deadline) void]{

  Checks if the current submission is within the deadline specified for the
  corresponding assignment. If the submission is past this deadline, a submit
  error is thrown. If no deadline is specified the check is trivially
  passed. This function is intended to be used in the pre-checker stage.

Deadlines are configured in the @filepath{config.rktd} file (see
  @secref{server-setup}), in a per-assignment basis, through the
  @indexed-racket{deadline} key. This is an optional keyword that holds an
  association list where each sublist has three elements:

@itemlist[

@item{the assignment name, which must coincide with the name used in the
@racket{active-dir} key}

@item{the deadline date, specified as a list @racket{(year month day hour minutes seconds)}}

@item{a non-negative integer indicating the amount of days allowed for late
submissions. If this value is @code{0} then the deadline is strict. Late
submissions are marked as such in the report and in the textual version of the
submission (if created).}

]

 Consider an example configuration:

@verbatim[#:indent 2]{ @;
(deadline (("Assignment 1" (2014 3 11 23 59 59) 3)))
}

}

@defproc[(check-max-submissions) void]{

If the assignment has a limited number of submissions, it checks whether the
user has already used up all of them. If the user has no submissions left, the
current one is rejected. This check is intended for use in the pre-checker stage.

By default users have an unlimited number of submissions. Restrictions on the
number of submissions must be defined in the @filepath{config.rktd} file,
through the optional @racket[max-submissions] key. This key must hold an
association where each sublist has two elements: the assignment name, and a
non-negative integer specifying the allowed submissions. For example:

@verbatim[#:indent 2]{@;
(max-submissions (("Assignment 1 5)))
}

@racket[check-max-submissions] only counts the number of existing successful
submissions, more specifically, it counts the number of @racket{"SUCCESS-n"}
directories specific to each user. Therefore, submissions that rejected by the
pre-checker or checker do not count against the submission limit.

}

@defproc[(update-submission-timestamp!) void]{@;

Sets the current timestamp as a thread-local value that can be later retrieved
using @racket[get-submission-timestamp]. This value can be then used to
identify submissions and related files, like reports or grading files. The
rationale is that we cannot use the @racket{"SUCCESS-n"} names because they
will change after new submissions.

}

@defproc[(get-submission-timestamp) string?]{@;

 Returns an string representing the latest timestamp value stored using
@racket[update-submission-timestamp!].

}

@defproc[(set-test-max-score! [max-score positive?]) void]{ @;

Sets @racket[max-score] as the maximum score possible for this submission, and
initializes the student's score to this value. Penalties are only applied to
the student's score, while the maximum score is available for reference only.

}

@defproc[(score-add-penalty! [penalty (or/c zero? positive?)]) void]{ @;

Discounts @racket[penalty] points from the student's score.

}

@defproc[(add-report-line! [line string?]) void]{ @;

Like @racket[add-header-line!], but it writes to the report instead of the
textual version of the submission.

}

@defform[(with-output-to-report expr ...)]{ @;

Evaluates @racket[expr ...] and redirects output to the report bound to this
submission. Similar to @racket[with-output-to-string] or
@racket[with-output-to-file].

}

@defproc[(add-score-to-report!) void]{ @;

An utility function, defined as:

@racketblock[
(define (add-score-to-report!)
  (add-report-line! (format "Final Score: ~a out of ~a"
                            (get-student-score)
                            (get-test-max-score))))
]
}

@defform[(write-report)]{ @;

Writes the report bound to the current submission into the root of the user
folder of the corresponding assignment (for example @filepath{Assignment
1/tester}). By default, the report is written immediately to this folder. It is
possible to configure the delay in the @filepath{config.rktd} file, using the
@racket[report-delay-in-minutes] key. The value of this key must be a
non-negative number, for example:

@verbatim[#:indent 2]{ @;
(report-delay-in-minutes 5)
}

Internally, the macro calls the @racket[start-timer] procedure in the following
way:

@racketblock[
(define-syntax (write-report stx)
   ...
   (define report-delay (get-report-delay-in-minutes))
   (start-timer (start-timer-manager) (* 60 report-delay) ...))
]

The default behavior is obtained because @racket[get-report-delay-in-minutes]
returns @racket[0] if the key is not found in @filepath{config.rktd}.

}

@defform*[((\@test test-desc error-desc expr result penalty)
           (\@test test-desc error-desc expr result equal? penalty))]{ @;

Like @racket[!test], but is designed to work with a report and a user score. It
evaluates @racket[(!test expr result)] or @racket[(!test expr result equal?)]
and redirects the output to the report. If the test fails, the error message is
written to the report and no exception is thrown, and the @racket[penalty] is
added to the score of the user.

}

@defform[(\@test/exn expr penalty)]{ @;

Like @racket[!test/exn] but the output is redirected to the report. In case of
error, the message is written to the report instead of raising an exception.

}
