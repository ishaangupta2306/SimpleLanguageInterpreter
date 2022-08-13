#lang racket
(provide (all-defined-out))

(define asrtt
  (lambda (dscrp exp)
    (if exp
        (cons 'PASSED dscrp)
        (cons 'FAILED dscrp))))

(define asrtf
  (lambda (dscrp exp)
    (if exp
        (cons 'FAILED dscrp)
        (cons 'PASSED dscrp))))

(define asrteq
  (lambda (dscrp exp1 exp2)
    (cond
      ((or (list? exp1) (list? exp2)) "Does not work for lists")
      ((eq? exp1 exp2) (cons 'PASSED dscrp))
      (else (cons 'FAILED dscrp)))))
