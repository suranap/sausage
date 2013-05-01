
;; @ Introduction

;; These are some standard compiler rules for Scheme:

;; dead local variable elimination will remove let variables that
;; aren't used in the body. This doesn't eliminate lambda formals that
;; are never used.

;; dead code elimination will remove code that doesn't do
;; anything. Abstractly, anything that is a pure function can be
;; removed. 

;; constant propagation will replace all variable references with a
;; constant. The variable definition will be removed by the dead local
;; variable rule. No need to do it here. If there's a set! anywhere,
;; don't propagate the constant.

;; constant folding will execute a function when it is pure and all
;; the arguments are constants. This should be done abstractly, rather
;; than manually writing rules for every function.





;; dead variable elimination
;;   This is strange. Because ANF form translates (begin . S*) into a
;;   series of nested let exprs, I'll just transform them back into
;;   begin exprs. Is that what I want?
(define dead-local-variable 
  (match-lambda
   (($ Let ($ Var V) E B a) (=> fail)
    (let* ((s (synth (scheme-attr B)))
	   (Vstr (symbol->string V)))
      (if (or (and-let* ((uses (m:member "use" s))) (b:member Vstr uses))
	      (and-let* ((sets (m:member "set" s))) (b:member Vstr sets)))
	  (fail)
	  (make-Begin E B a))))))


;; dead code elimination
;; - A few basic transformations to simplify code. 
(define dead-code
  (match-lambda 
   (($ If ($ Value V) C A) (if V C A))	; (if constant C A) => C or A
   (($ Set ($ Var X) ($ Var Y)) (=> fail) ; (set! x x) => 'unspecified
    (if (equal? X Y)
	unspecified
	(fail)))
   (($ Begin (or Var? Value?) E) E)	; (begin x E) => E
   ))


;; constant propagation
(define constant-propagation
  (match-lambda 
   (($ Let ($ Var V) (and ($ Value (not (or '* 'unspecified))) E) B a) (=> next)
    (print "Consider " V)
    (or ((manybu 
	  (match-lambda (($ Var V2) (=> fail) 
			 (if (equal? V V2) (begin (print "Replacing") E) (fail)))))
	 B)
	(next)))))