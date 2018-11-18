;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; list ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-table same-key?)
  (let ((local-table (list '*tables*)))

    ;; 从一维表格中返回匹配的键值对
    (define (my-assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (my-assoc key (cdr records)))))

    ;; 查询函数
    (define (lookup key-1 key-2)
      (let ((subtable (my-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (my-assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))

    ;; 插入函数
    (define (insert! key-1 key-2 value)
      (let ((subtable (my-assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (my-assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable (cons (cons key-2 value)
					   (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)

    ;; dispatch
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else "error operation --- TABLE" m)))

    dispatch))

