;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Queue ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;构造队列
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '()))
    (define (empty-queue?) (null? front-ptr)) ;栓查队列是否为空

    (define (insert-queue! item)        ;插入元素
      (cond ((empty-queue?) (let ((init_list (list item)))
                              (set! front-ptr init_list)
                              (set! rear-ptr  init_list)
                              front-ptr))
            (else           (let ((new-item (list item)))
                              (set-cdr! rear-ptr new-item)
                              (set! rear-ptr new-item)
                              front-ptr))))

    (define (delete-queue!)             ;删除元素
      (if (empty-queue?)
          (error "delete an empty queue!!!")
          (set! front-ptr (cdr front-ptr))))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)))
    dispatch))
