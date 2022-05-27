#lang racket
;Ivan Varenytsia
;COSI121B Spring 2022

; Stream preliminaries

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define head car)

(define (tail s) (force (cdr s))) (define stream-car car)

(define stream-cdr tail)

(define the-empty-stream (delay '()))

(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))

(define (map-stream fn S)
  (cons-stream (fn (head S))
              (map-stream fn (tail S))))

(define (filter-stream pred S)
  (if (pred (head S))
      (cons-stream (head S) (filter-stream pred (tail S)))
      (filter-stream pred (tail S))))

(define (1+ x) (+ 1 x))

(define (take n G)
  (if (= n 0)
      '()
      (cons (head G) (take (- n 1) (tail G)))))

(define (drop n G)
  (if (= n 0)
      G
      (drop (- n 1) (tail G))))

(define zeros (cons-stream 0 zeros))

(define (ints n) (cons-stream n (ints (+ n 1))))

(define integers (ints 0))

(take 10 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #0 - DONE
(define (series List)
  (if (null? List)
      zeros
      (cons-stream (car List) (series (cdr List)))))

;For testing
;(define ps (series '(3 1 4 1 5)))
;(take 10 ps)

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #1 - DONE
(define (sum s1 s2)
  (cons-stream (+ (stream-car s1)(stream-car s2)) (sum (stream-cdr s1) (stream-cdr s2))))

(define (star s1 s2)
  (cons-stream (* (stream-car s1)(stream-car s2)) (star (stream-cdr s1) (stream-cdr s2))))

;For testing
;(define ps1 (sum (series '(3 1 4 1 5 10 100)) (series '(2 7 1 8 2))))
;(define ps2 (star (series '(3 1 4 1 5 10 100)) (series '(2 7 1 8 2))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #2 - DONE
(define (zip-streams s1 s2)
  (cons-stream (cons (stream-car s1)(stream-car s2)) (zip-streams (stream-cdr s1) (stream-cdr s2))))

(define (new-sum s1 s2)
  (map-stream (lambda (x) (+ (head x) (tail x))) (zip-streams s1 s2)))

(define (new-star s1 s2)
  (map-stream (lambda (x) (* (head x) (tail x))) (zip-streams s1 s2)))

;For Testing
;(define ps (take 10 (zip-streams (series '(3 1 4 1 5 9)) (series '(2 7 1 8 2 8)))))
;(define ps3 (map-stream (lambda (x) (* (head x) (tail x))) (zip-streams (series '(3 1 4 1 5 9)) (series '(2 7 1 8 2 8)))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #3 - DONE
(define (scale c s1)
  (cons-stream (* (stream-car s1)c) (scale c (stream-cdr s1))))

;OR
;Same as 'scale', but uses map-stream
(define (scale2 c s1)
  (map-stream (lambda (x) (* c x)) s1))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Display Power Series
(define (show n p)
  (define (showit i p)
    (if (> i n)
        '()
        (cons (if (= i 0)
                  (head p)
                  (list '* (head p) 'z^ i))
              (showit (+ i 1) (tail p)))))
  (cons '+ (showit 0 p)))

;Problem #4 - DONE
;The way 'show' procedure operates, allows for an easy and quick fix to multiply a generating function by z.
(define (prod-z s1)
  (cons-stream 0 s1))

;For Testing
;(define ps1 (series '(3 1 4 1 5 9 2 6)))
;(tail (cons-stream 0 ps1))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #5 - DONE
(define (deriv s1)
   (star (tail s1) (ints 1)))

;For Testing
;(show 12 (deriv (series '(2 7 1 8 2 8 1 8 2 8 4 5))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #6 - DONE
(define (prod s1 s2)
  (cons-stream (* (head s1) (head s2))
               (sum (scale (head s1) (tail s2)) (prod (tail s1) s2))))

;For Testing
;(show 7 (prod (series '(1 1 1)) (series '(1 1 1 1 1))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #7 - DONE
(define (square s1) (prod s1 s1))

;The 'divide' procedure depends on 'reciprocal'
(define (divide s1 s2) 
   (prod s1 (reciprocal s2)))

(define (reciprocal s)
  (cons-stream 1 (scale -1 (prod (tail s) (reciprocal s)))))

;For Testing
;(show 10 (divide (series '(0 1)) (square (series '(1 -1)))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #8 - DONE
;repurposed the 'show' procedure to look for coefficient
(define (coeff n s1)
  (define (showit i p)
    (if (< i n)
        (showit (+ 1 i) (tail p)); if true
        (head p)))
  (showit 0 s1))

;For Testing
;(coeff 10 (divide (series '(0 1)) (series '(1 -1 -1))))

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;Problem #9 - DONE
;expt s1 n --> 'prod' repeated on s1 n-times
;Repurposed 'show' procedure again...
(define (expt s1 n)
  (define (showit i p x)
    (if (< i n)
        (showit (+ 1 i) (prod p x) x); if true
        p))
  (showit 1 s1 s1))

;For Testing
;(show 10 (expt (series '(1 1 1)) 5))
;(show 10 ( prod (series '(1 1 1))(prod (prod (series '(1 1 1)) (series '(1 1 1))) (prod (series '(1 1 1)) (series '(1 1 1))))))

(define (pascal n)
  (take (+ n 1) (map-stream (lambda (x) x)(expt (series '(1 1)) n))))

;For Testing
;(pascal 3)

(define (binomial n k)
  (if (>= n k) (coeff k (pascal n)) 0))

;For Testing
;(binomial 4 0)

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!!!!!!!!!!!!!!!
;Problem #10 - IDK how to do this one. I understand the concept, but not how to implement it.
(define (hat s1)
 (sum s1 (drop 1(sum s1 (drop 1(sum s1 (drop 1(sum s1 (drop 1(sum s1 (drop 1 (sum s1 (drop 1 s1))))))))))))
  )

(pascal 4)
(expt (series '(1 1)) 6)

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!!!!!!!!!!!!!!!
;Problem #11

(define golden-mean (/ (+ 1 (sqrt 5)) 2))