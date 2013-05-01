
;; @ Introduction

;; It appears that C++ template metaprogramming and multi-stage programming are very similar. A
;; program is written such that certain match&replace operations occur at compile-time. C++ template
;; metaprogramming is an interesting hack that depends on the compiler's ability to fold constant
;; expressions.

;; Multistage programming uses three annotations: brackets ".< >.", escapes ".~e", and run ".!". By
;; annotating code with this syntax, one can separate what happens at compile-time from run-time
;; code. The brackets indicate that a run-time term will be constructed, and the escape means the
;; value of the expression must be spliced into the bracketed term. Run calls the compiler on the
;; generated code fragment, which adds it as regular code. It looks like it might be a fancy
;; quasiquote package, but it is actually quite complicated to correctly write interesting
;; multistage programs.


;; A typical example for multistage programming is to partially evaluate exponentation with a
;; constant exponent. (expt x 5) ==> (let* ((t1 (* x x)) (t2 (* t1 t1))) (* x t2))

;; This stuff is verbose because I don't have a pretty rewrite rule facility.

(define expt-pe
  (match-lambda 
					; (expt x 0) ==> 0
   (($ App (and ($ Var 'expt) F) (X ($ Value 0)))
    (make-Value 0 '()))
					; (expt x 1) ==> x
   (($ App (and ($ Var 'expt) F) (X ($ Value 1)))
    X)
   
   (($ Let V ($ App (and ($ Var 'expt) F) (X ($ Value N))) B)
    (let ((T (gensym-Var 'expt)))
      (and (integer? N) (exact? N) (> N 1) 
	   (if (odd? N)			; (expt x 5) ==> (* (expt x 4) x)
					; (let ((T (expt X (- N 1)))) (let ((V (* T X))) B))
	       (make-Let T (make-App F (list X (make-Value (- N 1) '())) '())
			 (make-Let V (make-App (make-Var '* '()) (list T X) '())
				   B '())
			 '())
					; (expt x 4) ==> (let ((t (expt x 2))) (* t t))
					; (let ((T (expt X (/ N 2)))) (let ((V (* T T))) B))
	       (make-Let T (make-App F (list X (make-Value (/ N 2) '())) '())
			 (make-Let V (make-App (make-Var '* '()) (list T T) '())
				   B '())
			 '())))))))



