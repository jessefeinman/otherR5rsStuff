(define (list-head-tail list k)
  (let* ((list (append list '()))
         (tail (list-tail list k))
         (head list))
    (set-cdr! (list-tail list (- k 1)) '())
    (cons head (cons tail '()))))

(define (insert list ele k)
  (let* ((head-tail (list-head-tail list k))
         (head (car head-tail))
         (tail (cadr head-tail)))
  (append head (cons ele '()) tail)))

(define list '(1 2 3 4 5 6 7 8 9))
(insert list 99 5)

(define (my-list-head-iter list k)
  (define (iter head curr k)
    (cond ((zero? k) head)
          (else (iter
                 (append head (cons (car curr) '()) '())
                 (cdr curr)
                 (- k 1)))))
  (iter '() list k))

(define (my-list-head list k)
    (cond ((zero? k) '())
          (else (append (cons (car list) '()) (my-list-head (cdr list) (- k 1))))))
                
(define (my-list-tail list k)
  (cond ((zero? k) list)
        (else (list-tail (cdr list) (- k 1)))))

(define (insert-rec list ele k)
  (append (my-list-head list k) (cons ele '()) (my-list-tail list k)))

(insert-rec list 99 5)