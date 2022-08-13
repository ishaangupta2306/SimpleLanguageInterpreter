#lang racket
(provide (all-defined-out)) ; used for our testing file
(require "simpleParser.rkt")

; returns what the code in the file evaluates to
(define interpret
  (lambda (filename)
    (interpret-code (parser filename) '((return)))))

; abstractions for interpret-code
(define this-statement car)
(define next-statements cdr)

; actually interprets code using separate helper methods
(define interpret-code
  (lambda (code state)
    (cond
      ((null? code) (lookup-binding 'return state))
      (else (interpret-code (next-statements code) (m-state (this-statement code) state))))))

; abstractions for m-state's declare and assign helper functions
(define variable-name cadr)
(define variable-value (lambda (lis) (cdr (cdr lis))))

; changes the state
(define m-state
  (lambda (statement state)
    (cond
      ((declaration? statement) (declare (variable-name statement) (variable-value statement) state))
      ((assignment? statement) (assign (variable-name statement) (variable-value statement) state))
      ((return? statement) (return statement state))
      ((if-statement? statement) (if-statement statement state))
      ((while-loop? statement) (while-loop statement state))
      (else (error 'syntax-error)))))

; checks what type of statement is being called
(define statement-type car)
(define declaration? (lambda (lis) (eq? (statement-type lis) 'var)))
(define assignment? (lambda (lis) (eq? (statement-type lis) '=)))
(define return? (lambda (lis) (eq? (statement-type lis) 'return)))
(define if-statement? (lambda (lis) (eq? (statement-type lis) 'if)))
(define while-loop? (lambda (lis) (eq? (statement-type lis) 'while)))

; declares a variable - either with or without an initial value
(define declare ; 
  (lambda (var value state)
    (cond
      ; if a variable is only being declared and not initialized, add that variable as a list to state
      ((null? value) (cons (cons var '()) state))
      ; otherwise add that variable as a list with its value as well
      (else (cons (cons var (cons (m-value (extract-expression value) state) '())) state)))))

;(m-value '(<= x y) '((x 5) (y 6)))
;Helper method for assign that modifies the state
;Finds the variable in the state and updates it with the new value passed as parameter
;(old) state = '((x 12) (y 3) (z 4))
; var = 'y
; val = expression = '(* 3 5)
;(new) state = '((x 12) (y 15) (z 4))
(define modifyStateHelper
  (lambda (var val state)
    (cond
      ;If state is null, return null
      ((null? state) state)
      ;If car of list is state, then iterate car and cdr of the state
      ((list? (car state)) (cons (modifyState var val (car state)) (modifyState var val (cdr state))))
      ;If the variable is found in state, change its corresponding value (evaluated by m-value method) 
      ((and (eq? (car state) var) (not(error? (m-value val state) null))) (append (cons (car state) '()) (cons(m-value val state) '())))
      ;If the variable is found in state, change its corresponding value (evaluated by m-boolean method) 
      ((and (eq? (car state) var) (not(error? (m-value val state) null))) (append (cons (car state) '()) (cons(m-value val state) '())))
      ;Otherwise keep on searching
      ;((error? (m-value val state) (m-value val state)) (error 'using-before-declaring))
      (else (cons (car state) (modifyState var val (cdr state)))))))

; uses extract-expression to ensure modifyState works for all conditions
(define modifyState
  (lambda (var val state)
    (modifyStateHelper var (extract-expression val) state))) 

;Assigns a value to the variable & stores it in the state
;Helper methods - lookup-binding (for checking the existence of the variable)
;               - modifyState (for updating the value of the variable in the state)
(define assign
  (lambda (var val state)
    (cond
      ;If variable not found in state, throw an error
      ((boolean? (lookup-binding var state)) (error 'bad-assignment-error))      
      ;If variable is found but no corresponding value is found, then assign the new value
      ((error? (lookup-binding var state)null) (modifyState var val state))
      ;Otherwise variable & corresponding value is found, change the value
      (else (modifyState var val state)))))

;Return function
;Takes a list containing the return statement and the state
;returns a value / boolean
(define return
  (lambda (lis state)
    (assign 'return (m-value (extract-expression (cadr lis)) state) state)))

; if-statement abstraction
(define if_condition cadr)
(define truecase caddr)
(define falsecase cadddr)

; if statement. Executes the truecase if the condition is true, otherwise executes the falsecase
(define if-statement
  (lambda (lis state)
    (if (m-value (if_condition lis) state)
        (m-state (truecase lis) state)
        (m-state (falsecase lis) state))))

; abstraction for while-loop
(define condition (lambda (lis) (cadr lis)))
(define body (lambda (lis) (cdr (cdr lis))))

; while true change the state based upon the statement inside of the while loop
(define while-loop
  (lambda (lis state)
    (cond
      ((eq? (m-value (condition lis) state) #t) (while-loop lis (m-state (cons (body lis) '()) state)))
      (else state))))

;Helper methods for Abstraction for m-value
(define operator car)
(define left cadr)
(define right caddr)
;Evaluates the value of an expression which might contain variables stored in the state
;If the value of the variable is found in the state, expression returns the correct value
;Otherwise, it throws an error
(define m-value-helper
  (lambda (expression state)
    (cond
      ((null? expression) error' invalid-expression)
      ;If the expression is number then return it
      ((number? expression) expression)
        ;If the expression is 'true then return it 
      ((eq? expression 'true) expression)
      ;If the expression is 'false then return it 
      ((eq? expression 'false) expression)
       ;if expression is a variable and it's not found in the state then throw an error
      ((and (not (list? expression)) (boolean? (lookup-binding expression state))) error 'using-before-declaring)
       ;If expression is a variable and it exists in the state, then return its corresponding value
      ((not (list? expression)) (lookup-binding expression state))
       ;If expression exists with a correct operator and the expression has no error i.e. it's a valid expression
      ((and (eq? (operator expression) '+) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                           (+ (m-value (left expression)state) (m-value (right expression)state)))

      ((and (eq? (operator expression) '-) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (- (m-value (left expression)state) (m-value (right expression)state)))

      ((and (eq? (operator expression) '*) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (* (m-value (left expression)state) (m-value (right expression)state)))

      ((and (eq? (operator expression) '/) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (quotient (m-value (left expression)state) (m-value (right expression)state)))

      ((and (eq? (operator expression) '%) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (remainder (m-value (left expression)state) (m-value (right expression)state)))

      ((and (eq? (operator expression) '!) (not(error? (m-value (left expression)state) (m-value (left expression)state))))
                                                                  (not (m-value (left expression) state)))

      ((and (eq? (operator expression) '&&) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (and (m-value (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '||) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (or (m-value (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '==) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (eq? (m-value (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '!=) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (not (m-value  (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '>) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (> (m-value  (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '<) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (< (m-value  (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '>=) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (>= (m-value  (left expression) state) (m-value (right expression) state)))

      ((and (eq? (operator expression) '<=) (not(error? (m-value (left expression)state) (m-value (right expression)state))))
                                                                 (<= (m-value  (left expression) state) (m-value (right expression) state)))   
     
      (else (error 'bad-operation)))))

; uses extract-expression to ensure m-value works in all cases
(define m-value
  (lambda (expression state)
    (m-value-helper (extract-expression expression) state)))

;Takes a variable and a state and searches for variable's value
;If value found, returns the value, returns null
;If only variable is found and no corresponding value is found, throws a bad-state-error 
;returns a value / boolean / error
(define lookup-binding
  (lambda (variable state)
    (cond
      ;variable not found or state is null
      ((null? state) #f)
      ;If the car of state is list, search for the variable in car and cdr of the state
      ((list? (car state)) (or (lookup-binding variable (car state)) (lookup-binding variable (cdr state))))
      ;If the variable is found but no corresponding value, then return null
      ((and (eq? (car state) variable) (null? (cdr state))) error 'unitialized-variable)
      ;If the variable is found with its corresponding value, return the value
      ((eq? (car state) variable) (cadr state))
      ;Otherwise keep on searching
      (else (lookup-binding variable (cdr state))))))

;Checks if the method parameters are error messages
(define error?
  (lambda (a b)
    (cond
      ;If the method parameters are not error messages, just return false
      ((and (not (or (eq? a 'using-before-declaring) (eq? b 'using-before-declaring))) (not (or (eq? b 'invalid-expression) (eq? a 'invalid-expression))))#f)
      ;Otherwise return true
      (else #t))))

;if the expression is a car or a list holding one value that value inside the list is returned or that car
; is returned
; otherwise if it's a list of more than one value that whole list is returned
; this is so that once the m-value function is called
; if the input is an atom it'll return that atom or that atom's lookup reference
; if the input is a list it'll return that whole list
; (extract-expression a) -> a (extract-expression '(+ 3 5)) -> '(+ 3 5) (extract-expression '(a)) -> a
; (extract-expression '(1)) -> 1
(define extract-expression
  (lambda (expression)
    (cond
      ((not (list? expression)) expression)
      ((null? (cdr expression)) (car expression))
      (else expression))))