#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variables? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variables? exp) (symbols exp))
(define (same-variable? v1 v2)
  (and (variables? v1) (variables? v2) (eq? v1 v2)))

;;2.73写出和式和积式的求导过程
(define (make-sum a1 a2) (list '+ a1 a2))
(define (deriv-sum exp v)
  (make-sum (deriv (addend
