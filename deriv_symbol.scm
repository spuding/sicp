;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  sicp 2-56 2-57 2-58  ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;判断是否是符号
(define (variable? v1) (symbol? v1))

;;判断两个变量相同
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;和式的构造函数和选择函数
(define (sum? s)
  (and (pair? s) (eq? (car s) '+)))
;;取出被加数
(define (addend s) (cadr s))
;;取出加数
(define (augend s) (caddr s))
;;构造加式
(define (make-sum s1 s2)
  (cond ((=number? s1 0) s2)
	((=number? s2 0) s1)
	((and (number? s1) (number? s2) (+ s1 s2)))
	(else (list '+ s1 s2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;乘式的构造函数和选择函数
(define (product? s)
  (and (pair? s) (eq? (car s) '*)))
;;取出被乘数
(define (multiplier p) (cadr p))
;;取出乘数
(define (multiplicand p) (caddr p))
;;构造乘数
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

;;求导函数
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (multiplicand exp)
			(deriv (multiplier exp) var))))))
  
