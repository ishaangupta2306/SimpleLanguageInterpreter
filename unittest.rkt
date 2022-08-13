#lang racket
(require "interpreter.rkt")

; lookup-binding
(lookup-binding 'a '(a 12))
(lookup-binding 'a '((a 12) (b 3) (c 0)))
(lookup-binding 'a '((b 12) (a 3) (c 0)))
(lookup-binding 'a '((b 12) (c 3) (a)))
(lookup-binding 'a '((b 12) (d 3) (c 0)))
(lookup-binding 'a '((b 12) (d 3) (a)))

; m-value
( m-value '(+ x 10) '((e 3) (x 4) (a 1)))
( m-value '(+ (* a b) c) '((a 3) (b 4) (c 1)))

; modifyState
(modifyState 'x '(* 3 5) '((x 11) (b 3) (c 0)))
(modifyState 'x 12 '((a 11) (x 3) (c 0)))
(modifyState 'x 12 '((a 11) (b 3) (x 0)))

; assign
(assign 'x (+ (* 3 5) 12) '((x 11) (b 3) (c 0)))
(assign 'x 12 '((a 11) (x 3) (c 0)))
(assign 'x 12 '((a 11) (b 3) (x 0)))
;(assign 'x 12 '((a 11) (b 3) (c 0)))
(assign 'x 12 '((a 11) (b 3) (x)))

; while-loop
(while-loop '(while (< a 3) (= a (+ a 1))) '((a 1) (b 2) (c 3)))

; declare
(declare 'x '() '((a 1) (b 2) (c 3)))
(declare 'x '(10) '((a 1) (b 2) (c 3)))

; expression type
(declaration? '(var x))
(assignment? '(= x 10))
(return? '(return 10))
(if-statement? '(if (> x y) (return 10)))
(while-loop? '(while (> x y) (= x (+ x 1))))
(m-value? '(+ 1 2))
(m-boolean? '(&& true false))
