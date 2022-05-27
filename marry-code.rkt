#lang racket
;Ivan Varenytsia
;COSI121B Spring 2022
(define (match-make proposers proposees)
  (send proposers 'reset)
  (send proposees 'reset)
  (courtship proposers proposers proposees)
  (zip-together (send proposers 'name)
                (send (send proposers 'intended) 'name)))

 (define (zip-together list1 list2)
   (if (null? list1)
       '()
       (cons (list (car list1) (car list2))
	     (zip-together (cdr list1) (cdr list2)))))

 (define (filter pred lst)
   (cond ((null? lst) '())
	 ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
	 (else (filter pred (cdr lst)))))

;Problems #0 #4 #5 #7 arer in the pdf file.

;Implementation of procedures for problem #1:
(define (courtship unengaged-proposers proposers proposees)
  ;if everyone is engaged, stop
  (cond ((eq? '() unengaged-proposers) '())
        ;else make each person froom unengaed list propose to their top/or next best choice from their preference lists.
        (else ((car unengaged-proposers) 'propose)
              (courtship (currently-unengaged proposers) proposers proposees))))

;checks if two people are a couple.
(define (couple? person1 person2)
  (if (and (eq? ((person1 'intended) 'name) (person2 'name))(eq? ((person2 'intended) 'name) (person1 'name)))
      #t
      #f))

;iterates through each element of list and applies a predicate unengaged?
;I've used 'filter' procided in starter file
(define (currently-unengaged list-of-people)
  (filter unengaged? list-of-people))

;checks if a person instance passed as input has an empty list of 'intended 
(define (unengaged? person)
    (if (eq? '() (person 'intended)) 
        #t
        #f))

(define (send list-of-people message) 
  (cond ((null? list-of-people) '())
        (else (cons ((car list-of-people) message)
              (send (cdr list-of-people) message)))))

;I think that the original implementation of i-like-more? defined inside the make-person is quite complex
;and it also causes errors during the runtime. Because of that I have commented out the provided i-like-more?
;and wrote mine instead:
(define (i-like-more? person1 person2)
  (lambda (list) (> (length (memq person1 list)) (length (memq person2 list)))))
;I am aware than racket has inbuilt 'memq', but it hough it would be better for my learning process to actually write it out,
;I have used book pages 194-196 to write this -->
(define (memq item x) (cond ((null? x) false)
((eq? item (car x)) x) (else (memq item (cdr x)))))
;The assignment sheet says that we have to be vary of defining this procedure in corect context in order to
;have access to proper data structures. However, I noticed that i-like-more? is used/called only once in this
;code, and the prefereence list that we need is passed as a parameter such that ((i-like-more? person current-intended) preference-list),
;so the position of this procedure can be arbitrary as we are already provided with the data structure.

;The above implementation of i-like-more? assumes that the preference list applied to lambda is not empty.


(define (make-person my-name)
  (let ((name my-name)
        (preference-list '())
        (possible-mates '())
        (current-intended '()))
;This came with sample code from the original marry-code.rkt
;    (define (i-like-more? person1 person2)
;      (equal? (list person1 person2)
;              (filter (lambda (person)
;                        (or (eq? person person1) (eq? person person2)))
;                      preference-list)))
    (define (me message)
      (cond ((eq? message 'name) name)
            ((eq? message 'intended) current-intended)
            ((eq? message 'loves) preference-list)
            ((eq? message 'possible) possible-mates)
            ((eq? message 'reset)
               (set! current-intended '())
               (set! possible-mates preference-list)
               'reset-done)
            ((eq? message 'load-preferences)
               (lambda (plist)
                  (set! preference-list plist)
                  (set! possible-mates plist)
                  (set! current-intended '())
                  'preferences-loaded))
            ((eq? message 'propose)
               (let ((beloved (car possible-mates)))
                 (set! possible-mates (cdr possible-mates))
                 ;Message when 'me' proposes to 'beloved'
                 (write (list name 'proposes 'to (beloved 'name))) (newline)
                 (if (eq? ((beloved 'i-love-you) me)
                          'i-love-you-too)
                     (begin (set! current-intended beloved)
                            ;Message when 'beloved' accepts proposal from 'me'
                            (write (list (beloved 'name) 'accepts 'proposal 'from name)) (newline)
                            'we-are-engaged)
                     ;Message when 'beloved' rejects proposal from 'me'
                     (begin (write (list (beloved 'name) 'rejects 'proposal 'from name)) (newline)
                       'no-one-loves-me))))
;Problem #2 starts here -->
            ((eq? message 'i-love-you)
             (lambda (person)
               (if (eq? current-intended '())
                   ;no proposals --> accept anyways
                   (begin (set! current-intended person)
                   'i-love-you-too)
                   ;if has more than one option, check the preference list with i-like-more?
                   (if ((i-like-more? person current-intended) preference-list)
                       ;if likes new one more --> dump old, accept new
                       (begin ((current-intended 'i-changed-my-mind) me)
                              (set! current-intended person)  
                              'i-love-you-too)
                       ;else -->
                       'buzz-off-creep)
                   ))
             )
;Problem #2 ends here.
            ((eq? message 'i-changed-my-mind)
               (lambda (lost-love)
                  (cond ((eq? current-intended lost-love)
                            (set! current-intended '())
                            ;Message when 'lost-love' engages to someone else, and 'me' is now single.
                            (begin (write (list (lost-love 'name) 'dumps name)) (newline)
                            'dumped!))
                        (else (error 
                                 "Dumper must be engaged to dumpee! "
                                 name me lost-love)))))
            (else 
              (error "Bad message to a person " me name message))))

      me))


;Problem #7
(define bob (make-person 'Bob))
(define carol (make-person 'Carol))
(define ted (make-person 'Ted))
(define alice (make-person 'Alice))
((bob 'load-preferences) (list carol alice))
((ted 'load-preferences) (list alice carol))
((carol 'load-preferences) (list ted bob))
((alice 'load-preferences) (list bob ted))
(define men (list bob ted))
(define women (list carol alice))

(match-make men women);--> '((Bob Carol) (Ted Alice))
(match-make women men);--> '((Carol Ted) (Alice Bob))
;Results lead me to believe that the order of marriage proposal matters; the proposer, in this case, gets their priority.
;So, in this small case, the group that proposes first gets what they want/ gets who they like more.
;And if it’s true, I would rather propose marriage myself than have it proposed to me.