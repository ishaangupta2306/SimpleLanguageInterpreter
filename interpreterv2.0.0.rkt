#lang racket
; Bill Zhou, Ishaan Gupta
; v2.0.0

(provide (all-defined-out))
(require "simpleParser.rkt")

; global non-implementation-specific helpers (NISH)
(define contains?
  (lambda (val lis)
    (cond
      ((null? lis) #f)
      ((eq? (car lis) val) #t)
      (else (contains? val (cdr lis))))))

;Helper method to check if two entities are not equal
(define neq?
  (lambda (a b)
    (not (eq? a b))))

;Helper method to calculate additive-inverser of integer
(define additive-inverse
  (lambda (a)
    (* -1 a)))

;Helper method to check whether the entities are of same type
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
      ((null? code) (error' no-return-statement))
      ((eq? state 'error) 'error)
      (else (call/cc (lambda (break)
                       (interpret-code (next-statements code) (M-state (this-statement code) state break))))))))

; abstractions
(define restricted-keywords '(var = return if while begin))
(define bool-operators '(&& || > >= == != < <= !))
(define math-operators '(+ - * / %))

; checks what type of statement is being called
(define statement-type car)
(define declaration? (lambda (lis) (eq? (statement-type lis) 'var)))
(define assignment? (lambda (lis) (eq? (statement-type lis) '=)))
(define return? (lambda (lis) (eq? (statement-type lis) 'return)))
(define if-statement? (lambda (lis) (eq? (statement-type lis) 'if)))
(define while-loop? (lambda (lis) (eq? (statement-type lis) 'while)))
(define break? (lambda (lis) (eq? (statement-type lis) 'break)))
(define try? (lambda (lis) (eq? (statement-type lis) 'try)))
(define catch? (lambda (lis) (eq? (statement-type lis) 'catch)))
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
(define block? (lambda (lis) (eq? (statement-type lis) 'begin)))

; M-lookup -> number / boolean / error
(define M-lookup
  (lambda (var state)
    (cond
      ((list? (M-lookup* var state)) (error "error: variable has not been declared"))
      (else (M-lookup* var state)))))

;Helper method to call/cc the found 
(define M-lookup*
  (lambda (var state)
    (call/cc (lambda (break)
               (M-lookup-break var state break)))))

;Helper method to iterate through the state and find the binding
(define M-lookup-break
  (lambda (var state break)
    (cond
      ((null? state) '())
      ((list? (car state)) (cons (M-lookup-break var (car state) break) (M-lookup-break var (cdr state) break)))
      ((and (eq? (car state) var) (eq? (length state) 2)) (break (cadr state)))
      ((and (eq? (car state) var) (eq? (length state) 1)) (error "error: variable not assigned value"))
      (else (M-lookup-break var (cdr state) break)))))


; M-state -> state
(define M-state
  (lambda (statement state break)
    (cond
      ((return? statement) (break (M-return statement state)))
      ((declaration? statement) (M-state-declare statement state))
      ((assignment? statement) (vs-state (M-state-assign statement state)))
      ((break? statement) (M-state (cdr statement) state break))
      ((if-statement? statement) (M-state-if statement state break))      
      ((while-loop? statement) (M-state-while statement state break))
      ((block? statement) (M-state-block statement state break))
      (else state))))

; abstractions
(define condition cadr)
(define true-clause caddr)
(define false-clause cadddr)

; M-state-if -> state
;Method that returns state after performing if-else
(define M-state-if
  (lambda (statement state break)
    (cond
      ((eq? (vs-value (M-boolean (condition statement) state)) 'error) 'error)
      ((eq? (M-state (true-clause statement) state break) 'error) 'error)
      ((and (eq? (length statement) 4) (eq? (M-state (false-clause statement) state break) 'error))'error)
      ((vs-value (M-boolean (condition statement) state)) (M-state (true-clause statement) (vs-state (M-boolean (condition statement) state)) break))
      ((eq? (length statement) 4) (M-state (false-clause statement) (vs-state (M-boolean (condition statement) state)) break))
      (else state))))

; abstraction
(define loop-body caddr)
(define loop-block cdddr)

; M-state-while -> state
;Method that returns state after performing while-loop
(define M-state-while
  (lambda (statement state break)
    (cond
      ((eq? (vs-value (M-boolean (condition statement) state)) 'error) 'error)
      ((and (vs-value (M-boolean (condition statement) state))
            (eq? (loop-body statement) 'begin)) (M-state-while-block (loop-block statement) state break))
      ((vs-value (M-boolean (condition statement) state))
       (M-state-while statement (M-state (loop-body statement) (vs-state (M-boolean (condition statement) state)) break) break))
      
      (else (vs-state (M-boolean (condition statement) state))))))

;Helper method to execute block of loop-body
(define M-state-while-block
  (lambda (block state break)
    (cond
      ((null? block) state)
      ((eq? (car block) 'break) (break (cdr state)))
      ((eq? (car block) 'continue) (M-state-while-block (cdr block) state break))
      (else (M-state-while-block (cdr block) (M-state (car block) state break))))))

; abstractions
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;Method that performs unary operation
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
;Method that returns state after performing boolean operation
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

;;Method that returns state after performing value operations
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

;Helper method for block declaration
(define car-layer
  (lambda (state)
    (cond
      ((null? state) '())
      ((null? (car state)) '())
      ((list? (caar state)) (car-layer (car state)))
      (else state)
      )
    )
  )

;Helper method for block declaration 
(define mycons
  (lambda (a b)
    (cond
      ((eq? 'dontconsthis a) b)
      ((eq? 'dontconsthis b) a)
      (else (cons a b)))))

;Helper method for block declaration
(define cdr-layer
  (lambda (state)
    (cond
      ((null? state) 'dontconsthis)
      ((null? (car state)) (mycons (cdr-layer (car state)) (cdr state)))
      ((not (list? (caar state))) 'dontconsthis)
      (else (mycons (cdr-layer (car state)) (cdr state)))
      )
    )
  )

;Method that performs declaration and returns the state
(define M-state-declare
  (lambda (statement state)
    (M-state-declare-helper1 statement state state)))

;Helper method for block declaration
(define M-state-declare-helper1
  (lambda (statement state entire-state)
    (mycons (M-state-declare-helper2 statement (car-layer state) state) (cdr-layer state))))

; (var vs)
; -> ((var v) s)
(define declare
  (lambda (var vs)
    (cons (list var (vs-value vs)) (vs-state vs))))


; M-state-declare -> state
; M-state-declare -> state
(define M-state-declare-helper2
  (lambda (statement state entire-state)
    (cond
      ((M-declared? (variable statement) entire-state) (error "variable has already been declared"))
      ((eq? (length statement) 2) (cons (list (variable statement)) state))
      ((and (eq? (length statement) 3)
            (list? (value statement))) (cond
                                         ((assignment? (value statement)) (declare (variable statement) (M-state-assign (value statement) entire-state)))
                                         ((value-expression? (value statement)) (declare (variable statement) (M-value (value statement) entire-state)))
                                         ((bool-expression? (value statement)) (declare (variable statement) (M-boolean (value statement) entire-state)))))
      ((eq? (length statement) 3) (cons (list (variable statement) (value statement)) state)))))


; abstractions
(define returnval cadr)

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



; (var vs f)
; ->(v (f var v s))
;(define distribute
;  (lambda (var vs-pair msa-helper)
;    (list (vs-value vs-pair) (msa-helper var (vs-value vs-pair) (vs-state vs-pair)))))

;Helper method for assigning operation
;(define MSA-helper
;  (lambda (var newval state)
;    (cond
;      ((null? state) '())
;      ((list? (car state)) (cons (MSA-helper var newval (car state)) (MSA-helper var newval (cdr state))))
;      ((and (eq? (car state) var) (eq? (length state) 1)) (cons (list (car state) newval) (MSA-helper var newval (cddr state))))
;      (else (cons (car state) (MSA-helper var newval (cdr state)))))))

; M-state-assign -> (value state)
; Method that assings a value and returns the new state
;(define M-state-assign
;  (lambda (statement state)
;    (if (M-declared? (variable statement) state)
;        (cond
;          ((literal? (value statement)) (list (value statement) (MSA-helper (variable statement) (value statement) state)))
;          ((variable? (value statement)) (distribute (variable statement) (list (M-lookup (value statement) state) state) MSA-helper))
;          ((value-expression? (value statement)) (distribute (variable statement) (M-value (value statement) state) MSA-helper))
;          ((bool-expression? (value statement)) (distribute (variable statement) (M-boolean (value statement) state) MSA-helper))
;          ((assignment? (value statement)) (distribute (variable statement) (M-state-assign (value statement) state) MSA-helper))) 
;        (error "error: variable has not been declared"))))


; M-declared? -> boolean
; Helper method for checks for declaration
;(define M-declared?
;  (lambda (var state)
;    (cond
;      ((null? state) #f)
;      ((list? (car state)) (or (M-declared? var (car state)) (M-declared? var (cdr state))))
;      ((eq? (car state) var) #t)
;      (else (M-declared? var (cdr state))))))

; Metho that adds a  new layer for a block
(define add-state-layer
  (lambda (state)
    (cons '() state)))

(define M-state-block
  (lambda (statement state break)
    (cond
      ((eq? '() statement) (cdr-layer state))
      ((eq? 'begin (car statement)) (M-state-block (cdr statement) (add-state-layer state) break))
      (else (M-state-block (cdr statement) (M-state (car statement) state break) break)))))

; Call/cc for try-catch 
;(define try-catch
;  (lambda (ls state)
;    (call/cc (lambda (catch-break)
;               (try-block ls state catch-break)))))

; Method that peforms catch-block
;(define M-state-catch-block
;  (lambda (block state)
;    (cond
;      ((null? block) state)
;      ((list? (car block)) (cons (M-state-catch-block (car block) state) (M-state-catch-block (cdr block) state)))
;      ((eq? (car block) 'finally) (M-state-final-block block state))
;      (else (M-state block state)))))

;Method that performs the final-block
;(define M-state-final-block
;  (lambda (block state)
;    (cond
;      ((null? block) state)
;      ((list? (car block)) (cons (M-state-final-block (car block) state) (M-state-final-block (cdr block) state)))
;      (else (M-state block state)))))
      
;Abstraction
;(define cdr try-block)
;(define caddr catch)
;(define cddr catch-block)

;Method that performs the try-block
;(define M-state-try-block
;  (lambda (ls state catch-break)
;    (cond
;      ((null? ls) (catch-break '()))
;      ((list? ls) (M-state-try-block (try-block ls) state catch-break))
;      ((and (eq? (car ls) 'throw) (eq? catch catch?)) (catch-break (M-state-catch-block (catch-block ls) (cddr state))))      
;      (else (M-state-try-block (cdr ls) state catch-break)))))

