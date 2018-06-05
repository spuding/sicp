;;定义树叶
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;;定义huffman树
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;;解码
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;;根据比特位选择分支
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- choose-branch" bit))))

;;将树或树叶插入到集合set，set是上升集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

;;生成叶集合
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;;2.67解码sample-message
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;2.68编码message
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;;编码单个符号
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((l (left-branch tree)) (r (right-branch tree)))
	(cond ((symbol-exist? symbol l) (cons 0 (encode-symbol symbol l)))
	      ((symbol-exist? symbol r) (cons 1 (encode-symbol symbol r)))
	      (else (error "bad symbol or no symbol" symbol))))))

(define (symbol-exist? symbol tree)
  (define (exist? s t)
    (cond ((null? t) #f)
	  ((eq? s (car t)) #t)
	  (else (exist? s (cdr t)))))
  (exist? symbol (symbols tree)))

;;2.69生成huffman树
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
	((null? (cdr leaf-set)) (car leaf-set))
	(else (successive-merge (adjoin-set (make-code-tree (car leaf-set)
							    (cadr leaf-set))
					    (cddr leaf-set))))))

;;2.70编码song
(define rock '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))

(define song '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))
