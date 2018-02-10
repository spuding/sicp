;;2.1 实现有理数正负号处理
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> (* n d) 0)
	(cons (abs (/ n g)) (abs (/ d g)))
	(cons (- (abs (/ n g))) (abs (/ d g))))))

;;2.2 实现线段
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((head (start-segment s)) (tail (end-segment s)))
    (make-point (/ (+ (x-point head) (x-point tail)) 2)
		(/ (+ (y-point head) (y-point tail)) 2))))

;;2.3 实现矩形
(define (make-rectangle length1 width1)
  (cons length1 width1))

(define (rectangle-len r)
  (car r))
(define (rectangle_wid r)
  (cdr r))

(define (rec-area r)
  (* (rectangle-len r) (rectangle-wid r)))
(define (rec-perimeter r)
  (* (+ (rectangle-len r) (rectangle-wid r)) 2))

;;2.20 多个参数
(define (same-parity x . y)
  (define f (if (even? x)
		    even?
		    odd?))
  (define (iter srclist result)
    (cond ((null? srclist) (cons x (reverse result)))
	  ((f (car srclist)) (iter (cdr srclist) (cons (car srclist) result)))
	  (else        (iter (cdr srclist) result))))
  (display y)
  (iter y '()))


;;2.26 deep reverse
(define (deep-reverse ls0)
  (define (iter ls result)
    (cond ((null? ls)       result)
	  ((not (pair? (car ls))) (iter (cdr ls) (cons (car ls) result)))
	  (else                   (iter (cdr ls) (cons (deep-reverse (car ls)) result)))))
  (iter ls0 '()))

;;2.28 求树叶列表
(define (fringe tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree)) (fringe (cdr tree))))))

;;2.29 二叉活动体
(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (mobile-blance? mobile)
  (let ((l (* (branch-length (left-branch mobile))
	      (branch-weight (left-branch mobile))))
	(r (* (branch-length (right-branch mobile))
	      (branch-weight (right-branch mobile)))))
    (and (equal? l r)
	 (if (pair? (branch-structure (left-branch mobile)))
	     (mobile-blance? (branch-structure (left-branch mobile)))
	     #t)
	 (if (pair? (branch-structure (right-branch mobile)))
	     (mobile-blance? (branch-structure (right-branch mobile)))
	     #t))))

;;2.30 用两种方法平立树叶
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree2 sub-tree)
	     (square sub-tree)))
       tree))
  
;;2.31 tree-map
(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))

;;2.33 基本表操作看作累积的定义
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
	      0
	      sequence))

;;2.34 多项式
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;;2.35 用累积计算树叶
(define (my-count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x)
		     (if (pair? x)
			 (my-count-leaves x)
			 1))
		   t)))
			   
			   
;;2.36 ((1 2 3) (4 5 6) (7 8 9))->(12 15 18)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
	    (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;;2.37 矩阵运算
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))
	 m)))

;;2.40 简化嵌套映射
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))




		    
