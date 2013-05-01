
;; @ Introduction

;; These are low-level transformations typically found in an
;; optimizing compiler. Note that this operates on Core Scheme
;; *without* any datatypes, except for #f.


;; @ Beta Reduction

;; If V is not set! and V is used only in application position, then inline the lambda

(define beta-reduction
  (match-lambda 
   (($ Let V (and ($ Lambda Ps E) L) B) (=> fail)
    (let ((V? (lambda (s) (equal? V s))))
      (let ((newB
	     (manytd (match-lambda
		      (($ Set (? V?)) (fail))
		      (($ App (? V?) As) 
		       (fold-right make-Let E Ps As)))
		     B)))
	(and newB (make-Let V L (normalize newB))))))))


;; @ Dead Code Elimination

(define dead-code-elimination
  (match-lambda
   (($ Begin (or (? Value?) (? Var?) ($ App (? pure-function?))) E) E)
   (($ If ($ Value val) T E) (if val T E))
   ))

;; @ Simple Constant Propagation

;; When a variable is let-bound to a value

(define constant-propagation
  (match-lambda 
   (($ Let ($ Var V) E B a) (=> next)
    (or ((manybu 
	  (match-lambda (($ Var V2) (=> fail) 
			 (if (equal? V V2) E (fail)))))
	 B)
	(next)))))