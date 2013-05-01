
(define ord (m:new-order string=? string<?))

(define (use1 ord v) (m:insert ord (m:new-entry (symbol->string v) 1) (m:empty)))
(define (set1 ord v) (m:insert ord (m:new-entry (symbol->string v) 1) (m:empty)))

(define (count-def/use-merge e1 e2)
  (cons (+ (car e1) (car e2))
	(+ (cdr e1) (cdr e2))))


;; PROBLEM: See how I have to stick the new attribute in the child
;; node?  That's because the synthesize function overwrites the
;; current attribute with the "join" of the children. If I update the
;; Set node, for example, the synthesize function won't even look at
;; it.
(define count-def/use-rule 
  (match-lambda
   (($ Set ($ Var N a1) V2 a2) 
    (make-Set (make-Var N (set1 ord N)) V2 (set1 ord N)))
   (($ Let (and V ($ Var N a1)) E B a)
    (make-Let (make-Var N (set1 ord N)) E B a)) ; set1 applied to a child node
   (($ Var N a) 
    (make-Var N (use1 ord N))))) ; this one is OK because synthesize ignores it.


(define count-def/use-join (cut attribute-union ord count-def/use-merge <> <>))

(define count-def/use (new-Analysis "def/use" count-def/use-rule count-def/use-join 'bottomup))


;; ************************

;; HACK!!!
;; PUT a key-value pair in the map for a category. Stored as alist key -> ((category . value) ...)
(define (put map category key value)
  (let* ((e (m:member ord key map))
	 (ne (if e 
		 (m:new-entry key (acons category value (m:entry-value e)))
		 (m:new-entry key (acons category value '())))))
    (m:insert ord ne map)))

;; GET a val for a key's category; otherwise #f
(define (get map category key value)
  (and-let* ((e (m:member ord key map)) 
	     (a (assoc category (m:entry-value e))))
	    (cdr a)))


;; ************************

;; Do use & def separately 

(define (use-merge c1 c2) (+ c1 c2))

(define use-rule 
  (match-lambda 
   (($ Var N a) 
    (let ((a (if (null? a) (attribute m:empty m:empty) a)))
      (make-Var N (attribute (in a) (put (out a) 'use (symbol->string N) 1)))))))

(define use-analysis (make-Analysis

(define (def-merge c1 c2) (+ c1 c2))

(define def-rule 
  (match-lambda 
   (($ Set ($ Var N a1) R a) (make-Set (make-Var N (attribute (in a1) (set1 ord N))
   (($ Let ($ Var N _) _ _ _) (set1 ord N))))



;; **********************************

;; These rules are topdown 

;; ((lambda (x) B) y) == (let ((x y)) B)
(define beta-reduction-rule
  (match-lambda 
   (($ Let V (? Lambda? L) B a)
    (and (not-assigned? V (Scheme-attr B))
	 (put *in* 'constant-propagation V L)))
   (($ App ... ))))			; TODO: need to generate new names 


;; If V := E, and V is not assigned to in B, propagate V -> E.
(define constant-propagation-rule
  (match-lambda 
   ((and T ($ Let ($ Var V _) (? Value? E) B _))
    (and (not-assigned? V (Scheme-attr B))
	 (put *in* 'constant-propagation V E)
	 T))
   (($ Var V _) (get *in* 'constant-propagation V))))


;; If X := Y, and Y is a local variable, and X & Y are not assigned to in B, propagate X -> Y
(define copy-propagation-rule
  (match-lambda 
   (($ Let ($ Var X _) ($ Var Y _) B a)
    (and (local-variable? Y)
	 (not (assigned? X (Scheme-attr B))) ; not sure about these checks
	 (not (assigned? Y (Scheme-attr B))) ; stuck 'em in to be very safe
	 (put *in* 'copy-propagation X Y)))
   (($ Var V _) (get *in* 'copy-propagation V))))


;; If V := E, and V is not used, remove assignment
(define dead-assignment-rule
  (match-lambda 
   (($ Let ($ Var V _) (? Value? E) B _)
    (and (not (used V (Scheme-attr B)))
	 B))))

;; For dead-assignment, if E is a "pure" expression, it can be removed also


;; **************************

;; (lambda (x) (f x)) == f  
;; not really safe, apply only if f is known 
(define eta-expansion-rule 
  (match-lambda 
   (())))

(define (test n)
  (letrec ((odd (lambda (n) (if (> n 0) (even (- n 1)) 0)))
	   (even (lambda (n) (if (> n 0) (odd (- n 1)) 0))))
    (odd n)))

(lambda (n) 
  (if (> n 0) 
      (let ((n' (- n 1)))
	(if (> n' 0) 
	    (odd (- n' 1)) 
	    0)) 
      0))

(define (fact n)
  (define (fact' n acc)
    (if (> n 0) 
	(fact' (- n 1) (* n acc))
	1))
  (fact' n 1))

(define (fold p f i l)
  (let loop ((i i) (l l))
    (if (null? l) 
	i
	(let ((i (f (car l) i)))
	  (if (p i)
	      i
	      (loop i (cdr l)))))))


(define (analysis attribute tree)
  (fold (lambda (e t) (or ((Analysis-rule (m:entry-key e)) t) t))
	tree 
	(attribute tree)))

