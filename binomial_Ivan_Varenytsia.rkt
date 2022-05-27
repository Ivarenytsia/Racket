#lang racket
;Ivan Varenytsia
;Spring 2022

; This procedure, not to be used, marks where in the code you have to fill stuff in...

(define ***fill-in-your-code-here*** '())

;;;; Beginning...

(define (ints from to)
  (if (> from to)
      '()
      (cons from (ints (+ 1 from) to))))

; a tail recursive reverse

(define (reverse L)
  (rev L '()))

(define (rev L S)
  (if (null? L)
      S
      (rev (cdr L) (cons (car L) S))))

;;;; Exercise 2.4 
;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))
;Answer to 2.4
;(define (cdr z)
;  (z (lambda (p q) q)))

;;;; Exercise 2.22
;The first implementation produces a reverse order list because it takes a first selected element and then
;appends it to the empty list, then takes next element and appends it to that list. In that way, the firrsrt element
;is the last in the new list...
;Second implementation adds a number of unvanted parenthesis, this is because it keeps constructing new lists
;each time it recursively calls itself.

;;;; Exercise 2.25
;(1 3 (5 7) 9)
;(car (cdaddr (list 1 3 (list 5 7) 9))) ;returns 7
;((7))
;(caar (list (list 7))) ; returns 7
;(1 (2 (3 (4 (5 (6 7))))))
;(cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))) ; returns 7

;;;; Exercise 2.26
;(define x (list 1 2 3))
;(define y (list 4 5 6))

;(append x y) ; result - '(1 2 3 4 5 6)
;(cons x y) ; result - '((1 2 3) 4 5 6)
;(list x y) ; rersult - '((1 2 3) (4 5 6))

;;;; Exercise 2.27
;@@@@@@@
;@@@@@@@

;;;; Exercise 2.28
;I implemeent and use the solution to #2.28 at Excercise #8.

;;;; Exercise 0
;;;; Exercise 1
;;;; Exercise 2
;;;;See the pdf attached for solutions for #0 #1 and #2

;;;; Exercise 3 melding two trees
(define (meld a b)
  (if (null? a)
      b
      (cond
        [(and (number? a) (number? b)) (if (> a b) (append (z a) (z b)) (append (z b) (z a)))]
        [(> (car a) (car b)) (cons (car a) (xyz (cdr a) b))]
        [(> (car b) (car a)) (cons (car b) (xyz a (cdr b)))]
        )
      )
  )
;(meld '(4 (2 1) 3) '(14 (12 11) 13))

;I had some issues with applying length to a car of a single element, so
;i wrote this as an ugly fix to the code.
;it works)))
(define (numToLst a)
  (if (number? a)
      '(1)
      '(1 2))
  )

;this is another fix to my code, basically without this (meld 5 6) -> '(6 . 5)
;with this procedure (meld 5 6) -> '(6 5)
;its a terrible workaround the (cons 5 6), but it works)))
(define (z a)
  (if (number? a)
      (list a)
      '())
  )

(define (xyz a b)
  (if (null? a)
      b
        (cond
        [(and (= (length (numToLst(car a))) 1) (= (length (numToLst(car b))) 1) (> (car a) (car b))) (cons b a)]
        [(and (= (length (numToLst(car a))) 1) (= (length (numToLst(car b))) 1) (< (car a) (car b))) (cons a b)]
        [(and (> (length (numToLst(car a))) 1) (= (length (numToLst(car b))) 1) (> (caar a) (car b))) (cons b a)]
        [(and (> (length (numToLst(car a))) 1) (= (length (numToLst(car b))) 1) (< (caar a) (car b))) (cons a b)]
        [(and (= (length (numToLst(car a))) 1) (> (length (numToLst(car b))) 1) (> (car a) (caar b))) (cons b a)]
        [(and (= (length (numToLst(car a))) 1) (> (length (numToLst(car b))) 1) (< (car a) (caar b))) (cons a b)]
        [(and (> (length (numToLst(car a))) 1) (> (length (numToLst(car b))) 1) (> (caar a) (caar b))) (cons b a)]
        [(and (> (length (numToLst(car a))) 1) (> (length (numToLst(car b))) 1) (< (caar a) (caar b))) (cons a b)]
        [else (cons a b)]
        )))

;;;; Exercise 4  evenmeld
;I'm actually surprised it works because I spent god knows how long overthinking this problem,
;and its just four lines of code.
(define (evenmeld L)
  (if (null? L)
      '()
      (cons (meld (car L) (car (cdr L))) (evenmeld (cddr L)))
  ))

;;;; Exercise 5 trees
;I didn't quite understand the idea of this problem.
;But I think this is how its supposed to be.
(define (trees n)
  (if (null? n)
      '()
      (if (even? (length n))
          (cons (meld (car n) (car (cdr n))) (evenmeld (cddr n)))
          (cons (cons (car n) '()) (trees (cdr n)))
          )
      )
  )

(define (queue L)  (reverse (trees L)))

;;;; binary numbers

(define (binary n)
  (if (= n 0)
      (list 0)
      (bin n)))

(define (bin n)
  (if (= n 0)
      '()
      (if (even? n)
          (cons 0 (bin (/ n 2)))
          (cons 1 (bin (/ (- n 1) 2))))))

(define (decimal bs)
  (if (null? bs) 0 (+ (car bs) (* 2 (decimal (cdr bs))))))



;;;; Exercise 6 increment
;What it does is reverses the given binary number,
;and runs a simple algorithm that if the digit is 1 then add 0 to a new lst
;if the digit is 0 then add 1 to the new lst and finish the procedure because
;anything bbeyong that point will remain the same.
(define (increment x)
  (reverse(add+ (reverse x)))
  )
(define (add+ x)
  (if (= (car x) 1)
      (append '(0) (add+ (cdr x)))
      (append '(1) (cdr x))
      )
  )

;;;; Exercise 7  add

(define (plus a b)
  (if (< (length a) (length b))
      (add a b 0)
      (add b a 0)))

(define (add S L c)
  (if (= c 0)
      (if (null? S)
          ***fill-in-your-code-here***
          (if (= (car S) 0)
              ***fill-in-your-code-here***
              ; (car S)= 1
              (if (= (car L) 0)
                  ***fill-in-your-code-here***
                  ; (cdr L)= 1
                  ***fill-in-your-code-here***)))
      ; c= 1
      (if (null? S)
          ***fill-in-your-code-here***
          (if (= (car S) 0)
              ***fill-in-your-code-here***
              ; (car S)= 1
              (if (= (car L) 0)
                  ***fill-in-your-code-here***
                  ; (car L)= 1
                  ***fill-in-your-code-here***)))))

(define (check a b)
  (let ((as (binary a))
        (bs (binary b)))
    (let ((cs (plus as bs)))
      (write (list a '+ b '= (+ a b))) (newline)
      (write (list 'as '= as)) (newline)
      (write (list 'bs '= bs)) (newline)
      (write (list 'cs '= cs '=_10 (decimal cs))) (newline)
      cs)))

;;;; Exercise 8  max-queue
;max-queue first calls on procedure fringe(analogous to one from the textbook) to rermove the tree structure from the queue,
;then it calls on the max-lst to find the max value of the resulting list 
(define (max-queue Q)
  (max-lst (fringe Q)))

;This is similar to ex2.28 from the SICP book.
;Pretty much it returns a 'smoothed' list, removing all branches of the tree, loses structure not data, which is good in this case.
(define (fringe Q)
  (if (null? Q)
      '()
      (cond
;z is a procedure that I've defined at line ~95, here it turns a decimal into a single value list
        [(number? Q) (z Q)]
        [(append (fringe (car Q)) (fringe (cdr Q)))]
        )
      )
  )
;finds the max value in the given list
(define (max-lst L)
  (if (null? L)
      '()
      (cond
        [(null? (cdr L)) (car L)]
        [(< (car L) (cadr L)) (max-lst (cdr L))]
        [(max-lst (cons (car L) (cddr L)))]
        )
      )
  )


;;;; Exercise 9  insert
(define (insert x L)
  (cond ((null? L) (list x))
        ((and (number? (car L)) (< x (car L))) (cons x L))
        (else (cons (car L) (insert x (cdr L))))))

;;;; Exercise 10 find
;Works for most cases:
;If (find 1 Q), given Q from Ex#10, will return '(1) instead of 1.
;Other than that, returns the values if found.
;If not found, returns '().
(define (find v Q)
  ;(writeln Q) ; for testing purposes
  (if (null? Q)
      '()
      (if(number? (car Q))
         (if (eq? v (car Q))
             Q
             (find v (cdr Q)))
         (append (find v (car Q)) (find v (cdr Q)))
         )
      )
  )

;;;; Exercise 11 remove
(define (remove v Q)
  ;(writeln Q) ; for testing purposes
  (if (null? Q)
      '()
      (if(number? (car Q))
         (if (not(eq? v (car Q)))
             Q
             '())
         (append (remove v (car Q)) (remove v (cdr Q)))
         )
      )
  )

;;;; Exercise 12 merge

(define (merge Q1 Q2)
  ***fill-in-your-code-here***)

;;;; Exercise 13 remove-max

(define (remove-max Q)
  ***fill-in-your-code-here***)

(define (test Q)
   (write Q)
   (newline)
   (if (null? Q)
       '()
       (test (remove-max Q))))