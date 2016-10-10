; Jesse Feinman
; Jesse.Feinman@gmail.com
; bi-nary->n-ary function

; How to recursively pass arbitrary numbers of arguments recursively:
; Simple example, no stoping condition does not terminate:
(define (n-args-simple . args)
  (apply n-args (args)))

; Terminating example:
(define (n-args-terminating n . args)
  (display n)
  (display "\t")
  (display args)
  (display "\n")
  (cond ((> n 0) (apply n-args-terminating (cons (- n 1) args)))))
(n-args-terminating 5 'a 'b 'c 'd)

; Practical examples:
; Unary->n-ary:
(define (unary->n-ary proc)
  (define (iterator . args)
    (cond ((null? (cdr args))
           (proc (car args)))
          (else (proc (car args))
                (apply iterator (cdr args)))))
  iterator)
((unary->n-ary display) "a" "b" "c" "\n")

; Terminating example making use of above code:
(define (n-args-terminating-n-ary-display n . args)
  ((unary->n-ary display) n "\t" args "\n" )
  (cond ((> n 0) (apply n-args-terminating-n-ary-display (cons (- n 1) args)))))
(n-args-terminating-n-ary-display 5 'a 'b 'c 'd)

; Bi-nary->n-ary:
(define (bi-nary->n-ary proc)
  (define (iterator . args)
    (cond ((and (not (null? (cdr args)))
                (not (null? (cddr args))))
           (apply iterator (cons (proc (car args) (cadr args)) (cddr args))))
          ((and (not (null? (cdr args)))
                (null? (cddr args)))
           (proc (car args) (cadr args)))))
  iterator)

(define (times a b) (* a b))
(define (a^b a b) (expt a b))

(* 1 2 3 4 5 6)
; is equalivant to:
(* 1 (* 2 (* 3 (* 4 (* 5 6)))))
; is equalivant to:
((bi-nary->n-ary times) 1 2 3 4 5 6)

(expt (expt (expt (expt (expt 2 2) 3) 4) 5) 6)
; is equalivant to:
((bi-nary->n-ary expt) 2 2 3 4 5 6)

; Preconditions:
; >= 2 arguments are passed of a type which proc can process and make sense
; when combined like so: (proc (... (proc a1) ...) an), proc terminates,
; the number of arguments is finite.
; Proof:
; Invariant: the invariant is that the list of arguments, a are arguments
; remaining, b are arguments already processed.
; [b][a1 a2 ... an] where initially b is empty.
; Initially:
; [a1 a2 ... an] are passed and all are unprocessed. If there are exactly
; 2 arguments then the function returns proc applied to those arguments.
; If there are more than 2, then the function calls it's self again and
; replaces the first 2 arguments with the single argument (proc a1 a2).
; nth case:
; assume it's correct for the nth case
; n+1th case:
; [b][a1 a2 ... an] are passed, b is already processed and a is not. If
; there are exactly 2 arguments b and a1 then the function returns proc
; applied to those arguments. If there are more than 2, then the function
; calls it's self again and replaces the first 2 arguments with the single
; argument (proc a1 a2) which reduces the size of the unprocessed arguments
; by 1.
; Termination:
; Since the number of arguments passed is reduced by 1 every time and the
; number of arguments is initially a positive integer >= 2. By the properties
; of integers, if itt is repeatedly decremented by 1, it will eventually reach
; 1 arguments which causes a non-recursive base case to terminate the program.
; Postcondition:
; (bi-nary->n-ary) returns a procedure applies a binary procedure to an
; arbitrary number of arguments by recursively applying the binary procedure to
; the first 2 arguments, combining them into 1.