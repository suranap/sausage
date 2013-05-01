
;; @ Dataflow Engine

;; A dataflow engine simply walks up and down the tree propagating and
;; combining attributes. This engine, for now, is limited to Core
;; Scheme. To use, provide a join function and a function to compute
;; new attributes for specific nodes.

;; PROBLEM: The rule has to put attributes in the child node or *in*
;; variable because the dataflow functions can't look at the parent
;; node. That's an irritating design, ain't it?

;; PROBLEM: This stuff only works for one attribute at a time. I need
;; to propagate many attributes at the same time. The attributes
;; should be, I think, a record of <join, rule, direction, data>. That
;; way I can run them all at the same time. There is still a problem
;; of attributes that rely on other attributes. I can copy what
;; attribute grammars do to solve that (later).


;; HACK!!!

;; Keep both maps in a cons cell
(define attribute cons)
(define in car)
(define out cdr)

;; @ Analysis Data

;; name: string - for debugging, a string name
;; rule: tree -> tree - compute attributes for a node
;; join: attr attr -> attr - given two attribute values, merge them
;; direction: bottomup | topdown - in which direction should I run this?
(define-const-structure (Analysis name rule join direction))

;; Ordering rules
(define analysis-order 
  (m:new-order eqv? (lambda (x y) (string<? (analysis-name x) (analysis-name y)))))

;; The attribute field will now be a map of (Analysis -> data)


;; @ Bottom-Up Analysis

;; still doesn't work for different directions. The problem is when I
;; move up (or down), I create a new attribute map which loses all the
;; info from the other direction. Right now, I just reuse one of the
;; attribute maps and hope for the best, but really I need to do
;; something a bit smarter.
(define (join a1 a2)			; ignoring direction right now
  (fold (lambda (e m)
	  ;; TODO: both a1 and a2 must have the same list of analyses, else error
	  (let ((analysis (m:entry-key e)) 
		(data2 (m:entry-value e))
		(data1 (m:entry-value (m:member analysis-order analysis m))))
	    (m:insert analysis-order 
		      (m:new-entry analysis ((Analysis-join analysis) data1 data2))
		      m)))
	a1 a2))


;; There's a problem with Begin, which could have an empty S or E.
;; Given a join and rule (to assign dataflow values to specific nodes) function, 
(define synthesize
  (lambda (tree)
    (let ((t2 (fold (lambda (e t)
		      (or ((Analysis-rule (m:entry-key e)) t) t))
		    tree (Scheme-attr tree))))
      (match t2
	     (($ Define V B a)  (make-Define V B (attribute 
						  (in a) 
						  (join (Scheme-attr V) (Scheme-attr B)))))
	     (($ Set V V2 a)    (make-Set V V2 (attribute 
						(in a) 
						(join (Scheme-attr V) (Scheme-attr V2)))))
	     (($ Lambda P+ B a) (make-Lambda P+ B (attribute 
						   (in a) 
						   (fold join (Scheme-attr B) (map Scheme-attr P+)))))
	     (($ Begin S E a)   (make-Begin S E (attribute 
						 (in a) 
						 (join (Scheme-attr S) (Scheme-attr E)))))
	     (($ Let V E B a)   (make-Let V E B (attribute 
						 (in a) 
						 (join (Scheme-attr V) (join (Scheme-attr E) (Scheme-attr B))))))
	     (($ If C T E a)    (make-IF C T E (attribute 
						(in a) 
						(join (Scheme-attr C) (join (Scheme-attr T) (Scheme-attr E))))))
	     (($ App F A* a)    (make-App F A* (attribute 
						(in a) 
						(fold join (Scheme-attr F) (map Scheme-attr A*)))))
	     ((? Var? e) e)
	     ((? Value? e) e)))))


;; @ Top-Down Analysis

;; I'm going to use a gross, disgusting global variable to propagate
;; values down. I'd rather use a fluid, or get access to the parent.

(define *in* '())

;; Actually, there is no need for a join rule here because there's
;; only one parent. If I were using a control flow graph, then there
;; would be more inputs. I'll leave it like this until I can further
;; generalize it. 

(define (inherit join rule)
  (lambda (tree)
    (let ((t2 (or (rule tree) tree)))
      (match-lambda      
       (($ Define V B a)  (make-Define V B (attribute *in* (out a))))
       (($ Set V V2 a)    (make-Set V V2 (attribute *in* (out a))))
       (($ Lambda P+ B a) (make-Lambda P+ B (attribute *in* (out a))))
       (($ Begin S E a)   (make-Begin S E (attribute *in* (out a))))
       (($ Let V E B a)   (make-Let V E B (attribute *in* (out a))))
       (($ If C T E a)    (make-If C T E (attribute *in* (out a))))
       (($ App F A* a)    (make-App F A* (attribute *in* (out a))))
       (($ Var N a)       (make-Var N (attribute *in* (out a))))
       (($ Value D a)     (make-Value D (attribute *in* (out a))))))))


;; @ Helpers 

;; To help create a join function, this will merge two maps together
;; and execute a join function whenever there's an intersection.
(define (map-union ord join m1 m2)
  (define (f e m)
    (cond
     ((m:member ord (m:entry-key e) m) => 
      (lambda (c) (m:insert ord (m:new-entry (m:entry-key e) 
					     (join (m:entry-value e) (m:entry-value c)))
			    m)))
     (else (m:insert ord e m))))
  (m:fold f m1 m2))



