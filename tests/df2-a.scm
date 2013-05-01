
;; display tree + attributes

;; Keep both maps in a cons cell
(define attr cons)
(define inherit car)
(define synth cdr)

;; reset attributes
(define (reset-attributes empty-attr)
  (match-lambda
   (($ Define ($ Var V) B) (make-Define (make-Var V empty-attr) B empty-attr))
   (($ Set ($ Var V) V2)   (make-Set (make-Var V empty-attr) V2 empty-attr))
   (($ Lambda P* B)        (make-Lambda (map (match-lambda (($ Var N) (make-Var N empty-attr))) P*) 
					B empty-attr))
   (($ Begin S E)          (make-Begin S E empty-attr))
   (($ Let ($ Var V) E B)  (make-Let (make-Var V empty-attr) E B empty-attr))
   (($ If C T E)           (make-If C T E empty-attr))
   (($ App F A*)           (make-App F A* empty-attr))
   (($ Var N)              (make-Var N empty-attr))
   (($ Value D)            (make-Value D empty-attr))))


;; count uses of a variable

(define empty-bag (b:new (b:new-order string=? string<?)))
(define empty-attr 
  (let ((empty-map (m:new (m:new-order string=? string<?))))
    (attr empty-map empty-map)))

(define (increment-count category var attributes)
  (attr (inherit attributes)
	(m:insert category 
		  (b:insert (symbol->string var) empty-bag) 
		  (synth attributes))))

(define (remove-count category var attributes)
  (attr (inherit attributes)
	(m:insert category 
		  (m:remove (symbol->string var) 
			    (m:member category (synth attributes)))
		  (synth attributes))))

(define unspecified (make-Value 'unspecified empty-attr))

(define (join-count b1 b2) (b:fold b:insert b1 b2))

(define use
  (match-lambda 
   (($ Var N a) (make-Var N (increment-count "use" N a)))))

(define set
  (match-lambda
   (($ Set (and V ($ Var L)) V2 a)
    (make-Set V V2 (increment-count "set" L a)))))

;; ignore inherited attributes for now
(define (join-categories a1 a2)		; take two attribute DTs
  (attr (inherit empty-attr)		; ignore inherited attributes for now
	(m:fold (lambda (category bag a)
		  (cond 
		   ((m:member category a) ; merge this category
		    => (lambda (a-bag) (m:insert category (join-count bag a-bag) a)))
		   (else		; just add into a
		    (m:insert category bag a))))
		(synth a1) (synth a2))))


;; Actually, when merging use counts in an IF stmt, it should be the max of T and E, not the sum. 
(define (synthesized-attributes join)
  (match-lambda 
   (($ Define V B a)  (make-Define V B (join (Scheme-attr V) (Scheme-attr B))))
   (($ Set V V2 a)    (make-Set V V2 (join (Scheme-attr V) (Scheme-attr V2))))
   (($ Lambda P* B a) (make-Lambda P* B (l:fold join (Scheme-attr B) (map Scheme-attr P*))))
   (($ Begin S E a)   (make-Begin S E (join (Scheme-attr S) (Scheme-attr E))))
   (($ Let V E B a)   (make-Let V E B (join (Scheme-attr V) (join (Scheme-attr E) (Scheme-attr B)))))
   (($ If C T E a)    (make-IF C T E (join (Scheme-attr C) (join (Scheme-attr T) (Scheme-attr E)))))
   (($ App F A* a)    (make-App F A* (l:fold join (Scheme-attr F) (map Scheme-attr A*))))
   ((? Var? e) e)
   ((? Value? e) e)))



(define def
  (match-lambda
   (($ Define (and V ($ Var N)) B a)
    (make-Define V B (increment-count "def" N a)))
   (($ Lambda P* B a)
    (make-Lambda P* B 
		 (l:fold (lambda (V a)
			 (match-let ((($ Var N _) V)) (increment-count "def" N a)))
		       a P*)))
   (($ Let (and V ($ Var N _)) E B a)
    (make-Let V E B (increment-count "def" N a)))
   (($ Set (and V ($ Var N _)) E a)
    (make-Set V E (increment-count "def" N a)))))


(define (print . args) (for-each display args) (newline))


(define (debug-strategy s)
  (lambda (tree)
    (let ((t (s tree)))
      (if t (print "Rule: " tree " => " t))
      t)))


