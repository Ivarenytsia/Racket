#lang racket
;IVAN VARENYTSIA - SPRING 2022 - ivarenytsia@brandeis.edu
;; this is the code for problem set -- Lunar Lander

; Updating the ship state through one time unit when given a (constant) burn rate
;  You'll need to modify this procedure
;(define (update ship-state fuel-burn-rate)
;  (make-ship-state
;   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
;   (+ (velocity ship-state)
;      (* (- (* engine-strength fuel-burn-rate) gravity)
;         dt));velocity
;   (- (fuel ship-state) (* fuel-burn-rate dt))));fuel

; How to begin the "game"
;  You'll need to modify this procedure
;(define (play) (lander-loop (initial-ship-state)))

; Basic loop for the "game"
;  You'll need to modify this procedure
;(define (lander-loop ship-state)
 ; (show-ship-state ship-state) ; Display the current state
  ;(if (landed? ship-state) ; Run the next step 
   ;   (end-game ship-state)
    ;  (lander-loop (update ship-state (get-burn-rate)))))

; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!! WRITE YOUR NEW PROCEDURES HERE !!!!!!!!!!!!!!
; !!!!!! (this includes code-based exercise solutions!) !!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;SICP 1.17
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Given code/Code from the textbook:
(define (* a b) (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (even? n)
  (= (remainder n 2) 0))

;My code starts here:
;Fast product based on fast exponentiation
(define (double1 x) (* 2 x))

(define (halve x) (if (even? x) (/ x 2) (x)))

(define (** a b)
    (cond ( (or (= b 0) (= a 0)) 0)
          ((even? a) (double1 (** b (halve a))))
          ((even? b) (double1 (** a (halve b))))
          (else (+ a (** a (- b 1))))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;SICP 1.34
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Provided code:
(define (f g) (g 2))
;The issue with running (f f) is that the procedure f expects to receive a procedure or an expression in place of g
;that is compatible with inner argument. So when I pass f for g, it is incompatible with argument 2.
;In other words: (f f) --> (f 2) --> (2 2) and its neither an expression nor a procedure.

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;SICP 1.43
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Definition from class slides
(define (square x) (* x x))

;Definition from class slides
(define (compose f g)
  (lambda (x) (f (g x))))

;Definition from class slides
(define (repeated fn n)
  (if (= n 1)
      fn
      (compose fn (repeated fn (- n 1)))))
;All in all, i didnt do much here because all of the work
;was discussed and shown in class.

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;SICP 1.44
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;My code starts here:
;Here I define the avergae procedure, its a helper procedure.
(define (average x y z)
  (/ (+ x y z) 3)
  )

;Here I define the smooth procedure.
;This procedure computes an average of fn(x) f(x+dx) f(x-dx).
;To compute just f(x) set dx to 0.
(define (smooth fn dx)
  (lambda (x)
    (average (fn (- x dx))
             (fn x)
             (fn (+ x dx)))
    )
  )
;(smooth square 1)4) -->
;(3^2 + 4^2 + 5^2)/3 --> 16.6666666667

;Here I define the nfold procedure. ALternatively you can run (repeated (smooth fn dx) n)
;in the command line for same results. If n = 0, computes smooth procedure without folding
;else forlds smooth procedure n times.
(define (nfold n)
  ( if (= n 0)
       (lambda (fn dx) (smooth fn dx))
       (lambda (fn dx) (repeated (smooth fn dx) n))
  )
  )
;EXAMPLE: to run type something like: (((nfold 2) square 1)4)
;What this does is given a function (square 4), I can set up dx = 1 and smooth it two times:
;((repeated (smooth square 1)2)4) -->
;first smooth = (3^2 + 4^2 + 5^2)/3 --> 16.6666666667
;secod smooth = (15.6666666667^2 + 16.6666666667^2 + 17.6666666667^2)/3 = 278.444444446

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;FUNCTION DOUBLING:
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;LUNAR LANDER STARTS HERE
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #1: Added fuel-burn-rate constraint, such that the lander will not burn more fuel than it has.
;All changes are defined below, where the update and xx procedures are defined. I have decided to write
;another procedure that I named xx, and it pretty much constrains the fuel-burn rate. Although I understand
;that I could have used a lambda expression inside the update procedurre to do the same thing.
(define (update ship-state fuel-burn-rate)
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
   (+ (velocity ship-state)
      (* (- (* engine-strength (xx ship-state fuel-burn-rate)) gravity)
         dt));velocity
   (- (fuel ship-state) (* (xx ship-state fuel-burn-rate) dt))));fuel

(define (xx ship-state fuel-burn-rate)
  (cond ((<= fuel-burn-rate 0) 0)
        ((> fuel-burn-rate 0) (if (<= fuel-burn-rate 1)
                                  (cond((>=(fuel ship-state) fuel-burn-rate) fuel-burn-rate) (else (fuel ship-state)))
                                  (cond((>=(fuel ship-state) 1) 1) (else (fuel ship-state)))))))


(define (full-burn ship-state) 1)
(define (no-burn ship-state) 0)
(define (x ship-state) 0.5);for testing purposes
(define (y ship-state) 5);for testing
(define (z ship-state) -5);for testing

(define (ask-user ship-state) (get-burn-rate))

;Problem #2: Modified the play and lander-loop procedures. Play now can take in strategy as an input.
(define (play strategy)
  (lander-loop (initial-ship-state) strategy))

(define (lander-loop ship-state strategy)
  (show-ship-state ship-state)
  (if (landed? ship-state)
      (end-game ship-state)
      (lander-loop (update ship-state (strategy ship-state)) strategy)))

;Problem #3: Implemented the random choice procedure, where two startegies are inputted and one
;is chosen at random for each time interval of the game.
(define (random-choice x y)
  (lambda (ship-state)
    (if (= (random 2) 0)
        (x ship-state)
        (y ship-state))))

;Problem #4: Implemented a height-choice procedure. Here one strategy x is applied untill the ship
;is a bove a certain height h, then second strategy y is applied
(define (height-choice x y h)
  (lambda (ship-state)
    (if(>= (height ship-state) h) (x ship-state) (y ship-state))))

;Problem #5: Defined random-choice and height-choice using a choice compound strategy.
; I named these procedures new-random-choice and new-height-choice
(define (choice x y predicate)
  (lambda (ship-state)
    (if (predicate ship-state)
        (x ship-state)
        (y ship-state))))

(define (new-random-choice x y)
  (choice x y (lambda (ship-state) (= (random 2) 0))))

(define (new-height-choice x y h)
  (choice x y (lambda (ship-state) (>= (height ship-state) h))))

;Problem #6: Since the problem set did not specify the name for this procedure i called it
;unnamed-procedure-#6. So it does this "If the height is above 40 then do nothing.
;Otherwise randomly choose between doing a fullburn and asking the user"
(define (unnamed-procedure-#6)
  (choice no-burn ; does nothing until height >= 40
          (new-random-choice full-burn ask-user) ;after height < 40, randomly at each instant chooses bbetween full-burn or ask-user strategy
          (lambda (ship-state) (>= (height ship-state) 40))));this is a predicate for choice procedure

;Problem #7: See the attached Ivan_Varenytsia_PS1.pdf
;But the simple answer is that we have to use the equation for the motion of an object with uniform acceleration
;that is -> v_final^2 = v_initial^2 + 2*a*h
;we want v_final = 0, thus 0 = v_initial^2 = + 2*a*h
;moving parts around gets us to -> a = (-v_initial^2)/(2*h)
;which is clearly the equation given in the problem set -> a = (v^2)/(2*h);
;the only difference is that velocity is negative because the lander needs to
;accelerate in the direction oppoisite to the ground



;Problem #8:This only works when dt is less than 1. The assignment paper explains that dt has to be changed to
;a value less than 1 because the "strategy may crashes since it's a bad numerical approximation." - bottom of page 5
(define (acceleration ship-state)
  (/ (* (velocity ship-state) (velocity ship-state)) (* 2 (height ship-state)))) ;computes acceleration from the equation a = (v^2)/(2*h)

(define constant-acc 
  (lambda (ship-state)
    (+ (acceleration ship-state) gravity)));added gravity so that it would counter the next instance's pull

;Problem #9: See the screenshots submitted with the Ivan_Varenytsia_PS1.pdf.
;If you try to replicate the results, most likely, h = 30 and h = 20 will fail
;because in Problem#10 I limit the max fuel-burn-rate to 1, so at h = 20 the velocity cannot
;be countered by a burn of 1. Thus the ship crashes. However, on its own
;(height-choice no-burn constant-acc 20) and (height-choice no-burn constant-acc 30)
;land the ship when an arbitrary burn is allowed.

;Problem #10: I did it at the very top of this file (Problem#1). My work can be seen in xx procedure,
;xx procedure limits the burn rate to 0 <= burn-rate <= 1, and it also makes sure that the
;burn-rate is not greater than fuel reserves. I have decided to implement the constraint
;condition into the update procedure because in that way it will apply to any other burn strategy
;as well. So if we create a strategy Z = 10, the lander will still burn 1 at max because
;it is constrained so directly in the update procedure.

;Problem #11: this procedure lets the lander fall as long as possible, such that the burn-rate
;is "sufficiently less than 1"
(define (optimal-constant-acc ship-state)
    ( cond((<(- 1 (constant-acc ship-state)) 0.2) (constant-acc ship-state)) (else 0))
  ;works best for 0.2, although 0.25 is fine too. 0.3 and 0.4 get the "safest" landing, but use the most fuel
  )



; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!! WRITE NEW CODE ABOVE HERE !!!!!!!!!!!!!!!!!!!!!
; !!!!! (code below here is still useful to read and understand!) !!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

; Writing the ship's state to console
(define (show-ship-state ship-state)
  (write-line
   (list 'height (height ship-state)
         'velocity (velocity ship-state)
         'fuel (fuel ship-state))))

; Determining if the ship has hit the ground
(define (landed? ship-state)
  (<= (height ship-state) 0))

; Ending the game
(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
    (write-line final-velocity)
    (cond ((>= final-velocity safe-velocity)
           (write-line "good landing")
           'game-over)
          (else
           (write-line "you crashed!")
           'game-over))))

; Used in player-controlled burn strategy
(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))

; Starting state of the ship
(define (initial-ship-state)
  (make-ship-state
   50  ; 50 km high
   0   ; not moving (0 km/sec)
   20)); 20 kg of fuel left

; Global constants for the "game"
(define engine-strength 1) ; 1 kilonewton-second
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash
(define burn-key 32) ;space key
(define gravity 0.5) ; 0.5 km/sec/sec
(define dt 0.2) ; 1 second interval of simulation
;I have changed the dt per instructions at the bottom of page 5

; Getting the player's input from the console
(define (player-input)
  (char->integer (prompt-for-command-char " action: ")))


; You’ll learn about the stuff below here in Chapter 2. For now, think of make-ship-state,
; height, velocity, and fuel as primitive procedures built in to Scheme.
(define (make-ship-state height velocity fuel)
  (list 'HEIGHT height 'VELOCITY velocity 'FUEL fuel))
(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))
(define (second l) (cadr l))
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))
; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...
; for input and output
(define (write-line x)
  (display x)
  (newline))
(define (prompt-for-command-char prompt)
  (display prompt)
  (read-char))
; for random number generation
(#%require (only racket/base random))
; a ridiculous addendum
; (you’ll need this for the exercises)
(define (1+ x) (+ 1 x))