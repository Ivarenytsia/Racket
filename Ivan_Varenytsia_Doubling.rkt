#lang racket
(define (square x) (* x x))

(define (1+ x) (+ 1 x))

(define (double fn) (lambda (x) (fn (fn x))))

;((double 1+) 0)  --> (1+ (1+ 0)) --> (1+ 1) --> 2 --> 2^1

;(((double double) 1+) 0) --> ((double (double 1+)) 0) --> ((double 1+) ((double 1+) 0)) -->
;((double 1+) 2) --> (1+ (1+ 2)) --> (1+ 3) --> 4 --> 2^2

;Running ((((double double) double) 1+) 0) --> 16 --> 2^4
;And (((((double double) double) double) 1+) 0) --> 65536 --> 2^16

;Thus it makes sense to estimate ((((((double double) double) double) double) 1+) 0)
;to be 2^256 --> ~1.1579209e+77.

;My first though was that 2^256 cannot be computed in Racket implementation of Scheme
;because it has a hard limit on an integer size, such as 2^31, or something along those lines.
;However, after searching the web, I have come to a realization that Racket uses bignums
;and thus integer size is limited only by computer's memory.
;[https://stackoverflow.com/questions/55524870/what-is-the-largest-integer-number-value-in-scheme-racket/55525067#:~:text=2%20Answers&text=Most%20Scheme%20and%20Lisp%20implementations,limited%20only%20by%20available%20memory]
;Moreover, I wrote a procedure two-to-power which can do 2^256, giving a result of 115792089237316195423570985008687907853269984665640564039457584007913129639936.
;This further proves my belief, that Racket can handle 2^256.
(define (two-to-power x)
  (if (= x 0)
      1
      (* 2 (two-to-power (- x 1)))))
;So I think ((((((double double) double) double) double) 1+) 0) does not compute because of its sheer size. In case with my laptop,
;it can't be computed because it would take way too long to actually finish the computation.
;When I type (double(double)...) it causes a recursion on itself, expanding the length of needed operations
;in almost exponential scale.


