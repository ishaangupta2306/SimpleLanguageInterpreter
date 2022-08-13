#lang racket
; Bill Zhou, Ishaan Gupta, Jeff Kozik
; v1.1.0

(provide (all-defined-out))
(require "simpleParser.rkt")

; global non-implementation-specific helpers (NISH)
(define contains?
  (lambda (val lis)
    (cond
      ((null? lis) #f)
      ((eq? (car lis) val) #t)
      (else (contains? val (cdr lis))))))

(define neq?
  (lambda (a b)
    (not (eq? a b))))

(define additive-inverse
  (lambda (a)
    (* -1 a)))

(define same-type?
  (lambda (a b)
    (or (and (number? a) (number? b)) (and (boolean? a) (boolean? b)))))

; returns what the code in the file evaluates to
(define interpret
  (lambda (filename)
    (interpret-code (parser filename) '((return) (true #t) (false #f)))))

; abstractions
(define this-statement car)
(define next-statements cdr)
(define display-wrapper (lambda (stdout) (cond ((eq? stdout #t) 'true) ((eq? stdout #f) 'false) (else stdout))))

; actually interprets code using separate helper methods
(define interpret-code
  (lambda (code state)
    (cond
      ((null? code) (display-wrapper (M-lookup 'return state)))
      ((eq? state 'error) 'error)
      (else (interpret-code (next-statements code) (M-state (this-statement code) state))))))

; abstractions
(define restricted-keywords '(var = return if while))
(define bool-operators '(&& || > >= == != < <= !))
(define math-operators '(+ - * / %))

; checks what type of statement is being called
(define statement-type car)
(define declaration? (lambda (lis) (eq? (statement-type lis) 'var)))
(define assignment? (lambda (lis) (eq? (statement-type lis) '=)))
(define return? (lambda (lis) (eq? (statement-type lis) 'return)))
(define break? (lambda (lis) (eq? (statement-type lis) 'break)))
(define if-statement? (lambda (lis) (eq? (statement-type lis) 'if)))
(define while-loop? (lambda (lis) (eq? (statement-type lis) 'while)))
(define math-operator? (lambda (atm) (contains? atm math-operators)))
(define bool-operator? (lambda (atm) (contains? atm bool-operators)))
(define variable? (lambda (atm) (not (or
                                      (list? atm)
                                      (literal? atm)
                                      (contains? atm restricted-keywords)
                                      (math-operator? atm)
                                      (bool-operator? atm)))))
(define literal? (lambda (atm) (or (number? atm) (boolean? atm))))
(define value-expression? (lambda (lis) (or (math-operator? (statement-type lis))
                                            (variable? (statement-type lis))
                                            (number? (statement-type lis)))))
(define bool-expression? (lambda (lis) (or (bool-operator? (statement-type lis))
                                           (variable? (statement-type lis))
                                           (boolean? (statement-type lis)))))

; M-lookup -> number / boolean / error
(define M-lookup
  (lambda (var state)
    (cond
      ((null? state) (error "error: variable has not been declared"))
      ((and (eq? (caar state) var) (eq? (length (car state)) 2)) (cadar state))
      ((and (eq? (caar state) var) (eq? (length (car state)) 1)) (error "error: variable not assigned value"))
      (else (M-lookup var (cdr state))))))

; abstractions
(define variable-name cadr)
(define variable-value (lambda (lis) (caddr lis)))

; M-state -> state
(define M-state
  (lambda (statement state)
    (cond
      ((return? statement) (M-state-return statement state))
      ((declaration? statement) (M-state-declare statement state))
      ;((break? statement) (M-state-
      ((assignment? statement) (vs-state (M-state-assign statement state)))
      ((if-statement? statement) (M-state-if statement state))
      ((while-loop? statement) (M-state-while statement state))
      (else state))))

; abstractions
(define condition cadr)
(define true-clause caddr)
(define false-clause cadddr)

; M-state-if -> state
(define M-state-if
  (lambda (statement state)
    (cond
      ((eq? (vs-value (M-boolean (condition statement) state)) 'error) 'error)
      ((eq? (M-state (true-clause statement) state) 'error) 'error)
      ((and (eq? (length statement) 4) (eq? (M-state (false-clause statement) state) 'error))'error)
      ((vs-value (M-boolean (condition statement) state)) (M-state (true-clause statement) (vs-state (M-boolean (condition statement) state))))
      ((eq? (length statement) 4) (M-state (false-clause statement) (vs-state (M-boolean (condition statement) state))))
      (else state))))

; abstraction
(define loop-statement caddr)
(define loop-body cdddr)
(define loop-body-begin? caddr)

;(while (< a 6) (begin (= a (+ a 1)) (= b (+ b 2))) break)
'((var a 2) (var b 6) (while (< a 6) (begin (= a (+ a 1)) (= b (+ b 2)))) (return b))
; M-state-while -> state
(define M-state-while
  (lambda (statement state)
    (cond
      ((eq? (vs-value (M-boolean (condition statement) state)) 'error) 'error)
      ((and (vs-value (M-boolean (condition statement) state))
            (eq? (loop-body-begin? statement) 'begin)) (M-state-while-block-helper (loop-body statement) state))
      ((vs-value (M-boolean (condition statement) state))
       (M-state-while statement (M-state (loop-body statement) (vs-state (M-boolean (condition statement) state)))))      
      (else (vs-state (M-boolean (condition statement) state))))))

(define M-state-while-block-helper
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((declaration? statement) (M-state-while-block-helper (cdr statement) (M-state-declare statement state)))
      ((assignment? statement) (M-state-while-block-helper (cdr statement) (vs-state (M-state-assign statement state))))
      ((if-statement? statement) (M-state-while-block-helper (cdr statement) (M-state-if statement state)))
      ;break statement
      (else (error 'asfda)))))
      
      
     

; abstractions
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; (- (7 ((a 1) (b 2))))
; (-7 ((a 1) (b 2)))
(define state-unary-op
  (lambda (op vs-pair)
    (list (op (vs-value vs-pair)) (vs-state vs-pair))))

; operate on two value-state pairs
; (op l r f s)
; (v1, s1) = (f l s)
; (v2, s2) = (f r s1)
; ->(v1 v2 s2)
(define state-pair-op
  (lambda (operator leftoperand rightoperand M-class state)
    ((lambda (v1vs2) (list (operator (car v1vs2) (vs-value (cadr v1vs2))) (vs-state (cadr v1vs2))))
     ((lambda (vs1) (list (vs-value vs1) (M-class rightoperand (vs-state vs1)))) (M-class leftoperand state)))))

; redefining boolean operators to prevent syntax error
(define myand (lambda (a b) (and a b)))
(define myor (lambda (a b) (or a b)))
(define mynot (lambda (a) (not a)))
  
; M-boolean -> (boolean state)
(define M-boolean
  (lambda (statement state)
    (cond
      ((boolean? statement) (list statement state))
      ((variable? statement) (list (M-lookup statement state) state))
      ((variable? (car statement)) (list (M-lookup (car statement) state) state))
      ((boolean? (car statement)) (list (car statement) state))
      ((assignment? statement) (M-state-assign statement state))
      ((eq? (operator statement) '&&) (state-pair-op myand (leftoperand statement) (rightoperand statement) M-boolean state))
      ((eq? (operator statement) '||) (state-pair-op myor (leftoperand statement) (rightoperand statement) M-boolean state))
      ((eq? (operator statement) '>) (state-pair-op > (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '>=) (state-pair-op >= (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '==) (if (or (boolean? (leftoperand statement))
                                              (and (variable? (leftoperand statement)) (boolean? (M-lookup (leftoperand statement) state)))
                                              (and (list? (leftoperand statement)) (bool-expression? (leftoperand statement))))
                                          (state-pair-op eq? (leftoperand statement) (rightoperand statement) M-boolean state)
                                          (state-pair-op eq? (leftoperand statement) (rightoperand statement) M-value state)))
      ((eq? (operator statement) '!=) (if (or (boolean? (leftoperand statement))
                                              (and (variable? (leftoperand statement)) (boolean? (M-lookup (leftoperand statement) state)))
                                              (and (list? (leftoperand statement)) (bool-expression? (leftoperand statement))))
                                          (state-pair-op neq? (leftoperand statement) (rightoperand statement) M-boolean state)
                                          (state-pair-op neq? (leftoperand statement) (rightoperand statement) M-value state)))
      ((eq? (operator statement) '<=) (state-pair-op <= (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '<) (state-pair-op < (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '!) (state-unary-op mynot (M-boolean (leftoperand statement) state))))))

(define M-value
  (lambda (statement state)
    (cond
      ((number? statement) (list statement state))
      ((variable? statement) (list (M-lookup statement state) state))
      ((variable? (car statement)) (list (M-lookup (car statement) state) state))
      ((number? (car statement)) (list (car statement) state))
      ((assignment? statement) (M-state-assign statement state))
      ((eq? (operator statement) '+) (state-pair-op + (leftoperand statement) (rightoperand statement) M-value state))
      ((and (eq? (length statement) 3) (eq? (operator statement) '-)) (state-pair-op - (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '*) (state-pair-op * (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '/) (state-pair-op quotient (leftoperand statement) (rightoperand statement) M-value state))
      ((eq? (operator statement) '%) (state-pair-op remainder (leftoperand statement) (rightoperand statement) M-value state))
      ((and (eq? (length statement) 2) (eq? (operator statement) '-)) (state-unary-op additive-inverse (M-value (leftoperand statement) state))))))

; abstractions
(define variable cadr)
(define value caddr)

; (var vs)
; -> ((var v) s)
(define declare
  (lambda (var vs)
    (cons (list var (vs-value vs)) (vs-state vs))))

; M-state-declare -> state
(define M-state-declare
  (lambda (statement state)
    (cond
      ((M-declared? (variable statement) state) (error "variable has already been declared"))
      ((eq? (length statement) 2) (cons (list (variable statement)) state))
      ((and (eq? (length statement) 3)
            (list? (value statement))) (cond
                                         ((assignment? (value statement)) (declare (variable statement) (M-state-assign (value statement) state)))
                                         ((value-expression? (value statement)) (declare (variable statement) (M-value (value statement) state)))
                                         ((bool-expression? (value statement)) (declare (variable statement) (M-boolean (value statement) state)))))
      ((eq? (length statement) 3) (cons (list (variable statement) (value statement)) state)))))

; M-state-return -> state
(define M-state-return
  (lambda (statement state)
    (vs-state (M-state-assign (list '= 'return (M-return statement state)) state))))

; abstractions
(define returnval cadr)

; (define variable cadr)
; (define value caddr)

; M-return -> boolean / number
(define M-return
  (lambda (statement state)
    (cond
      ((literal? (returnval statement)) (returnval statement))
      ((variable? (returnval statement)) (M-lookup (returnval statement) state))
      ((value-expression? (returnval statement)) (vs-value (M-value (returnval statement) state)))
      ((bool-expression? (returnval statement)) (vs-value (M-boolean (returnval statement) state)))
      (else 'error))))

; abtractions
; (define variable cadr)
; (define value caddr)
(define vs-value car)
(define vs-state cadr)

; (var vs f)
; ->(v (f var v s))
(define distribute
  (lambda (var vs-pair msa-helper)
    (list (vs-value vs-pair) (msa-helper var (vs-value vs-pair) (vs-state vs-pair)))))

; M-state-assign -> (value state)
(define M-state-assign
  (lambda (statement state)
    (if (M-declared? (variable statement) state)
        (cond
          ((literal? (value statement)) (list (value statement) (MSA-helper (variable statement) (value statement) state)))
          ((variable? (value statement)) (distribute (variable statement) (list (M-lookup (value statement) state) state) MSA-helper))
          ((value-expression? (value statement)) (distribute (variable statement) (M-value (value statement) state) MSA-helper))
          ((bool-expression? (value statement)) (distribute (variable statement) (M-boolean (value statement) state) MSA-helper))
          ((assignment? (value statement)) (distribute (variable statement) (M-state-assign (value statement) state) MSA-helper))) 
        (error "error: variable has not been declared"))))

; M-state-assign helper -> state
(define MSA-helper
  (lambda (var newval state)
    (cond
      ((null? state) '())
      ((eq? (caar state) var) (cons (list var newval) (MSA-helper var newval (cdr state))))
      (else (cons (car state) (MSA-helper var newval (cdr state)))))))

; M-declared? -> boolean
(define M-declared?
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((eq? (caar state) variable) #t)
      (else (M-declared? variable (cdr state))))))

;(M-state-while '(while (< a 6) (begin (= a (+ a 1)) (= b (+ b 2))))'((a 4) (b 1)))
