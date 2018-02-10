;;1.8牛顿法求立方根
(define (sqrt3-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt3-iter (improve guess x)
		  x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (sqrt3 x)
  (sqrt3-iter 1.0 x))

;;1.11 n<3 f(n)=n; n>=3 f(n)=f(n-1)+2f(n-2)+3f(n-3)
;;递归计算过程
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;;迭代计算过程
(define (f2 n)
  (f2-iter 0 1 2 n))
(define (f2-iter a b c n)
  (cond
   ((< n 2) n)
   ((= n 2) c)
   (else (f2-iter b c (+ (* 3 a) ( * 2 b) c) (- n 1)))))

;;1.12杨辉三角形
(define (y n m)
  (cond
   ((or (= n 1) (= m 1) (= m n)) 1)
   (else (+ (y (- n 1) (- m 1)) (y (- n 1) m)))))

;;1.16求b的n次幂
(define (fast-iter b n)
  (define (f-iter b n a)
    (cond ((= n 0) a)
	  ((even? n) (f-iter (square b) (/ n 2) a))
	  (else (f-iter b (- n 1) (* a b)))))
  (f-iter b n 1))

;;1.17求两个数的乘积 递归
(define (*-rec a b)
  (cond ((= b 0) 0)
	((even? b) (double (*-rec a (/ b 2))))
	(else (+ a (*-rec a (- b 1))))))

;;1.18求两个数的乘积 迭代
(define (*-iter a b)
  (define (*-i a b product)
    (cond ((= b 0) product)
	  ((even? b) (*-i (double a) (halve b) product))
	  (else (*-i a (- b 1) (+ product a))))))

;;1.29采用辛普森规则求定积分
(define (sps f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (mysum f add-h b n (/ h 3) 0))

(define (mysum f a next b n p k)
  (if (> a b)
      0
      (cond ((= k 0)   (+ (* p (f a))    (sum f (next a) next b n p (+ k 1))))
            ((= k n)   (+ (* p (f a))    (sum f (next a) next b n p (+ k 1))))
	    ((even? k) (+ (* 2 p (f a))  (sum f (next a) next b n p (+ k 1))))
	    (else      (+ (* 4 p (f a))  (sum f (next a) next b n p (+ k 1)))))))
		 
(define (cube x)
  (* x x x))
;;版本2
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sps2 f a b n)
  (define h (/ (- b a) n))
  (define (p k) (cond ((or (= k 0) (= k n)) 1)
		      ((even? k)   2)
		      (else        4)))
  (define (g x) (* (p x) (f (+ a (* x h)))))
  (define (next x) (+ x 1))


  (* (/ h 3) (sum g 0 next n)))

;;1.31高阶计算过程 乘积
(define (product f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

(define (product2 f a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (f a)))))
  (iter a 1))

(define (pi-product n)
  (define (g x) (/ (* 4 x (+ x 1))
		   (square (+ (* 2 x) 1))))
  (define (inc x) (+ x 1))
  (* 4 (product2 g (exact->inexact 1) inc n)))

;;1.32 accumulate:乘积和求和的高阶函数
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-ac term a next b)
  (accumulate + 0 term a next b))
(define (product-ac term a next b)
  (accumulate * 1 term a next b))

;;1.33 filter的概念；比accumulate更普遍的概念
(define (filtered-accumulate condition? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(if (condition? a)
	    (iter (next a) (combiner (term a) result))
	    (iter (next a) result))))
  (iter a null-value))

;;1.35 利用不动点函数，计算黄金分割率
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (best-devide)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
       
;;1.37 无穷连分式,n d为函数，k为下标
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))
;;迭代
(define (cont-frac2 n d k)
  (define (iter i sum)
    (if (< i 0)
	sum
	(iter (- i 1) (/ (n i) (+ (d i) sum)))))
  (iter k 0))
