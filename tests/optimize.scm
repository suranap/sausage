
;; @ Introduction

;; These rules should apply strictly to Scheme syntax without 

;; These rules ignore any attributes. For now, I'm only propagating
;; the control flow path back to program entry. To function correctly,
;; the rule should adjust the path attribute, then rerun the
;; inherited-path rule to fix the path.

;; TODO: I'm probably using (=> fail) too often. It's OK to return #f
;; when a rule fails. Use fail when you want to retry another pattern.

;; TODO: In some places I use equal? to compare two variable names. I
;; should use a smarter equality that checks thru aliases.

(define call/cc call-with-current-continuation)

;; @ Basic Scheme optimizations

;; Dead Code elimination

;; The transformed code will need to be cleaned up using these simple
;; rules. Library specific rules shouldn't go here (for example, (if
;; (not V) C A) => (if V A C)).

(define dead-code-elimination
  (match-lambda
   (($ Begin (or (? Value?) (? Var?) ($ App (? pure-function?))) E) E)
   (($ If ($ Value val) T E) (if val T E))
   ))


;; @@ Dead Variable Elimination

;; If a variable is neither used nor re-defined by set!, then there's
;; no reason to create it. This might not be a good idea, though. In
;; fact, the Begin stmt block might not be a good idea! It makes it
;; harder to recreate the control flow.

;; Maybe I can limit this rule where E is a constant or variable. Then
;; I can throw out E and just pass back B. On the other hand, that
;; will happen anyway with the other rules.

(define dead-variable-elimination
  (match-lambda
   (($ Let V E B) 
    (and (not (use-variable? V B))
	 (not (def-variable? V B))
	 (make-Begin E B '())))))


;; @@ Useless Variable elimination

;; The generated code has lots of useless temporary variables that
;; should be cleaned up. This rule is quite conservative; can I
;; eliminate V even if E is used in B? Is it possible to write code
;; that changes E without changing V? If not, then this rule can be
;; relaxed. 

(define useless-variable-elimination
  (match-lambda
   (($ Let (and ($ Var var) V) (? Var? E) B)
    (and (not (def-variable? V B)) 
	 (not (def-variable? E B))
	 ((manybu (match-lambda 
		   (($ Var name) (and (equal? var name) E)))) 
	  B)))))


;; @@ Constant propagation

;; This is the simple, obvious constant propagation rule. If V is a
;; constant and it's not redefined anywhere, then replace all
;; occurences with the constant. However, this doesn't handle
;; constant propagation through IF:

;;      (let (x 1) (if b (begin (set! x 2) x) x))

;; If I checked the path from each exit term back to this Let for a
;; Set!, then I could move the constant above. 

(define constant-propagation
  (match-lambda
   (($ Let (and ($ Var sym) V) (? Value? C) B) 
    (and (not (def-variable? V B))
	 ((manybu (match-lambda 
		   (($ Var name) (and (equal? sym name) C))))
	  B)))))


;; @@ Common Sub-Expression Elimination

;; A pure function returns the same result given the same arguments;
;; you could safely memoize a pure function. This rule *should* be
;; quite powerful for a functional language, especially when all the
;; type check functions are inlined.

;; TODO: Partial CSE

(define common-subexpression-elimination
  (match-lambda 
   (($ Let V (and ($ App (and ($ Var (? pure-function?)) F) A*) E) B) 
    (if (not (def-variable? V B))
	(let ((new-B ((manybu 
		       (match-lambda (($ App fname args) (and (arg-equal? F fname)
							      (l:every arg-equal? A* args)
							      V ))))
		      B)))
	  (and new-B (make-Let V E new-B '())))))))


;; @ Helpers

;; Is there at least one use of variable V in B?
(define (use-variable? V B)
  (let ((var (Var-name V)))
    (call/cc				; abort when V is found
     (lambda (exit)
       ((oncetd (match-lambda 
		 (($ Var name) (=> fail) 
		  (if (equal? var name) (exit #t) (fail)))))
	B)))))

;; Is there at least one set! of variable V in B
(define (def-variable? V B)
  (let ((var (Var-name V)))
    (call/cc				; abort when V is found
     (lambda (exit)
       ((oncetd (match-lambda 
		 (($ Set ($ Var name) _) (=> fail) 
		  (if (equal? var name) (exit #t) (fail)))))
	B)))))


;; Is name a pure function? A pure function is one which produces the
;; same output given the same input parameters. Here, I also mean it
;; doesn't perform any side-effects on the parameters. Basically, I
;; can do this replacement: 
;;      (begin (pure x) (display x)) => (display x)

(define *pure-functions* (make-symbol-table))
(define (pure-function! name) (table-set! *pure-functions* name #t))
(define (pure-function? name) (table-ref *pure-functions* name))



(define (arg-equal? X Y)
   (or (and (Var? X) (Var? Y) (equal? (Var-name X) (Var-name Y)))
       (and (Value? X) (Value? Y) (equal? (Value-data X) (Value-data Y)))))