#lang racket

(require "p.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Project Tests"

   ; arithmetic functions test

   (check-equal? (eval-exp (plus (num 2) (num 2))) (num 4) "test1")
   (check-equal? (eval-exp (minus (num -5) (num 2))) (num -7) "test2")
   (check-equal? (eval-exp (mult (num 5) (num 2))) (num 10) "test3")
   (check-equal? (eval-exp (div (num -5) (num -2))) (num 2) "test4")
   (check-exn exn:fail?
              (lambda () (eval-exp (div (num 5) (num 0)))
              "test5"))

   (check-equal? (eval-exp (plus (num -5) (neg (minus (num -2) (num 3)))))
                           (num 0) "test6")

   (check-equal? (eval-exp (neg (mult (num 5) (plus
                                                (div (num -2) (num 3))
                                                (minus (num 1) (num -1)
                                                       )))))
                           (num -10) "test7")
   (check-exn exn:fail?
              (lambda () (eval-exp (plus (num 5.2) (num 0.0)))
              "test8"))

   ))


(require rackunit/text-ui)
;; runs the test
(run-tests tests)

