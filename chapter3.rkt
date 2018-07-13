;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; 第三章习题 ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang sicp

;;3.1 累加器
;;(define A (make-accumulator 5))
;;(A 10) -> 15
;;(A 10) -> 25
(define (make-accumulator value)
  (lambda (num)
    (set! value (+ value num))
    value))

;;3.2统计出某个给定过程被调用的次数
;;(define s (make-monitored sqrt))
;;(s 100)              -> 10
;;(s 'how-many-calls?) -> 1
;;(s 'reset-count)     -> 0
(define (make-monitored func)
  (let ((count 0))
    (define (how-many-calls?)
      count)
    (define (reset)
      (set! count 0))
    (define (mf num)
      (begin (set! count (+ count 1))
             (func num)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset) (reset))
            ((number? m) (mf m))
            (else (error "bad m:" m))))
    dispatch))
    
;;3.3 修改make-account,加上账号密码保护
(define (make-account balance sercet-passwd)
  (let ((bad-sercet-count 0))
    (define (withdraw amount)  ;;取款
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))

    (define (deposit amount)   ;;存款
      (set! balance (+ balance amount))
      balance)

    (define (right-passwd? passwd)
      (eq? passwd sercet-passwd))

    (define (dispatch pw m)
      (if (right-passwd? pw)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "unknown request" m)))
          (begin (display "serect error\n")
                 (set! bad-sercet-count (+ bad-sercet-count 1))
                 (if (>= bad-sercet-count 3)
                     (display "call-the-cops\n")
                     (display "again\n")))))
                           

    dispatch))


