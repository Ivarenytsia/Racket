;;;; REQUIRED CODE FOR PROBLEM SET 7 OF 
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;; It contains the metacircular evaluator from Chapter 4
;;;; (sections 4.1.1-4.1.4) and the lazy evaluator from section 4.2
;;;; of Structure and Interpretation of Computer Programs

;;;; Matches code in ch4.scm except that "eval" is "mc-eval"
;;;; and "apply" is "mc-apply".
;;;; Also includes enlarged primitive-procedures list

;;;; This file can be loaded into Scheme as a whole.
;;;; Then you can initialize and start the evaluator by evaluating
;;;; (driver-loop).

;;;;;;  ********** READ THE FOLLOWING CAREFULLY! **********

;;;; To work with the eager evaluator (the way Scheme works as we've used it), comment out
;;;; or delete everything after the line which reads ";;;; Modifying the evaluator" -- this is
;;;; line 410 in this file.

;;;; You should solve Exercises 4.6, 4.7, and 4.13 with the *eager* evaluator, and it is
;;;; encouraged that you "carry forward" your enhancements (let, let*, make-unbound) so that
;;;; they work in your hybrid evaluator as well.

;;;;;;  ********** READ THE PREVIOUS CAREFULLY! **********

;;;;  To run without memoization, reload the first version of force-it below

;;; Add stuff to make it work with DrRacket
; pretty print things
(#%require (only racket/pretty pretty-display))
(define pp pretty-display)

; error messages
(#%require (only racket/base error))

; Our scheme implements 1+, -1+ as primitive procedures
(define (1+ x) (+ x 1))
(define (-1+ x) (- x 1))

; Our scheme uses true and false, not #t and #f
(define true #t)
(define false #f)

;;; From section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'zero? zero?)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '1+ 1+)
        (list '-1+ -1+)
        (list 'quotient quotient)
        (list 'remainder remainder)
        (list '/ /)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


(define the-global-environment (setup-environment))

(pp "To start the metacircular evaluator, evaluate (driver-loop)")

'METACIRCULAR-EVALUATOR-LOADED

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; Your answers to Exercise 4.6, 4.7, and 4.13 goes here.

;; Exercise 4.6 #kind of works...
;(let ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩)) ⟨body⟩) -> (let ((⟨varn) . . . (varn)) ⟨body⟩)
;So in this exercise i followed the format from the book, and manipulated the expression into lambda...
(define (let->combination exp) 
   (cons (list 'lambda (map car (varn exp)) (body exp)) 
         (map cadr (varn exp)))) 


(define (body exp)(cddr exp))
(define (varn exp) (cadr exp))
(define (let? exp) (tagged-list? exp 'let))


;; Exercise 4.7
;I've used some of the definitions from 4.6
(define (append-lets arg bdy) (cons 'let (cons arg bdy)))
(define (let-list var) (list (car var)))

(define (let*->nested-lets exp) 
   (define (reduce var bdy) 
     (if (null? var) 
         bdy 
         (append-lets (let-list var) (list (reduce (cdr var) bdy))))) 
   (reduce(varn exp) (body exp))) 


;; Exercise 4.13
;similar to structure on page 514?
(define (make-unbound! var env) 
     5) 
;????????????????

;; Exercise 4.15
;See the pdf file.

;; Continue with the answers to Problem 1, 2, and 3 after the code below.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;;SECTION 4.2.2

;;; Modifying the evaluator

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ;((let? exp)(mc-eval (let->combiantion exp)env));addition for ex. 4.6
        ;((let*? exp)(mc-eval (let*->nested-lets exp)env));addition for ex. 4.7
        ;((unbound? exp)(mc-eval (make-unbound! exp)env));addition for ex. 4.13
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((cons? exp) (internal-cons exp env));non primitive cons, now the evaluator can handle internal-cons
        ;                                  that does not force the immediate evaluation of its argumeents
        ((application? exp)             ; clause from book
         (mc-apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (mc-eval exp env)))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'zero? zero?)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '1+ 1+)
        (list '-1+ -1+)
        (list 'quotient quotient)
        (list 'remainder remainder)
        (list '/ /)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
        ))

(define (delay? exp) (tagged-list? exp 'delay))

(define (thunkify exp) (make-lambda '() (list exp)))

'LAZY-EVALUATOR-LOADED
;(driver-loop)
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; Your answers to Problem 1, 2, and 3 after the code below.
;I've updated the definition of 'primitive-procedures' by adding moore items to the list.

;;Problem#1 modifications

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (del-delayed (procedure-parameters procedure))
           (list-of-delayed-args arguments env (procedure-parameters procedure))
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (del-delayed element)
  (cond ((null? element) element)
        ((delay? (car element)) (cons (cadar element) (del-delayed (cdr element))))
        (else (cons (car element) (del-delayed (cdr element))))))

;modified delay? from line 587
(define (delay? exp)
  (tagged-list? exp 'delayed))

(define (list-of-delayed-args exps env param)
  (if (no-operands? exps)
      '()
      (cons (if (delay? (car param)) (delay-it (first-operand exps) env)
                   (actual-value (first-operand exps) env))
            (list-of-delayed-args (rest-operands exps) env (cdr param)))))

;;Problem #2
;This new definition of if, called 'new-if' behaves like the regular 'if'.
;Pretty much it delays the excecution of x or y, until it evaluates the predicate.

;(define (new-if predicate (delayed x) (delayed y))
;  (cond (predicate x)
;        (else y)))

;Alternatively, I can cheat around the definition of "new-if", and define it as a funnel
;for the regular 'if':
;(define (new-if predicate (delayed x) (delayed y))
;  (if predicate x y))

;Definition of factorial from the problem set.
;(define (factorial n) new-if (= n 1) 1 (* n (factorial (- n 1)))))

;;Problem #3
;I've added ((cons? exp) (internal-cons exp env)) line to the mc-eval on line 461.

;I could have defined 'cons?' earlier, but its just helpful to have it where I can see it.
(define (cons? exp) (tagged-list? exp 'cons))

;Used on line 461 of the mc-eval.
(define (internal-cons exp env)
  (cons (mc-eval (cadr exp) env) (mc-eval (caddr exp) env)))

;Definition of cons as a nonprimitive procedure with delayed arguments.
;(define (new-cons (delayed a) (delayed b)) (cons a b))

;;My mc-evaluator is a bit unpredictable when it comes to the evaluation of
;multiple line procedures. So for example:
;(define (integers-from n)
;  (new-cons n (integers-from (+ n 1)))
;Will not compute(enter an infinite loop)
;while:
;(define (integers-from n) (new-cons n (integers-from (+ n 1))))
;works just fine.

;So for testing my solution for problem#3, the input has to be as follows:
;definition of nonprimitive 'cons' procedure ->
;(define (new-cons (delayed a) (delayed b)) (cons a b))
;definitions from the assignment pdf file ->
;(define (integers-from n) (new-cons n (integers-from (+ n 1))))
;(define integers (integers-from 1))
;(car integers)
;evaluates to 1
;(car (cdr integers))
;evaluates to 2

'UPDATED-LAZY-EVALUATOR-LOADED
;(driver-loop)
