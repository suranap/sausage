
;; Yet another attempt at an attribute evaluation engine. 

;; @ Attributed tree

;; Define the structure of the attributed syntax tree:

;; synthesized attributes are available from the children nodes. Only
;; the inherited attributes need be to pushed down the tree.

(define pack cons)
(define parent car)			; ptr to parent node
(define attr cdr)			; the computed attributes for this node


;; @ 

;; Finish this later. Hand code it for now. 
;(define (propagate-inherited-attribute tree)
;  (match tree
;	 (($ Define V B o)  (make-Define (copy V o) (copy B o) o))
;	 (($ Set V V2 o)    (make-Set (copy V o) (copy V2 o) o))
;	 (($ Lambda P* B o) (make-Lambda (copy P* o) (copy B o) o))
;	 (($ Begin S E o)   (make-Begin S E (update o)))
;	 (($ Let V E B o)   (make-Let V E B (update o)))
;	 (($ If C T E o)    (make-If C T E (update o)))
;	 (($ App F A* o)    (make-App F A* (update o)))
;	 (($ Var V o)       (make-Var V (update o)))
;	 (($ Value D o)     (make-Value D (update o)))))




;; @ Sample rules 

;; top-down
(define constant-propagation2
  (match-lambda 
   (($ Let V (? Value? E) B a) (=> fail)
    ;; ensure V not set in B
    (if (> (set-count a V B) 0)
	(fail)
    ;; add map V -> E
	(make-Let V E B (const a V E))))
   (($ Var V a) (=> fail)
    ;; return map V 
    (or (member (parent a) "const" V) (fail)))
    ))

;; top-down
(define constant-propagation 
  (match-lambda 
   (($ Let V (? Value? E) B a) (=> fail)
    ;; ensure V not set in B
    (if (member (attr (Scheme-attr B)) "set" (Var-name V))
	(fail)
    ;; add map V -> E
	(make-Let V E B 
		  (pack (parent a) (insert (attr a) "const" (Var-name V) E)))))
   (($ Var V a) (=> fail)
    ;; return map V 
    (or (member (parent a) "const" V) (fail)))
    ))

;; bottom-up
(define variable-set-count
  (match-lambda 
   (($ Set V R a)
    ;; add bag V
    (make-Set V R (pack (parent a) (insert (attr a) "set" V)))
    )))

;; bottom-up
(define variable-use-count
  (match-lambda 
   (($ Var V a)
    ;; add bag V
    (make-Var V (pack (parent a) (insert (attr a) "use" V)))
    )))

;; @ Helpers

;; Copy a structure, but update inherited ptr
(define (copy node)
  (let ((update (lambda (o) (pack a (attr o)))))
    (match tree
	   (($ Define V B o)  (make-Define V B (update tree)))
	   (($ Set V V2 o)    (make-Set V V2 (update tree)))
	   (($ Lambda P* B o) (make-Lambda P* B (update tree)))
	   (($ Begin S E o)   (make-Begin S E (update tree)))
	   (($ Let V E B o)   (make-Let V E B (update tree)))
	   (($ If C T E o)    (make-If C T E (update tree)))
	   (($ App F A* o)    (make-App F A* (update tree)))
	   (($ Var V o)       (make-Var V (update tree)))
	   (($ Value D o)     (make-Value D (update tree))))))
