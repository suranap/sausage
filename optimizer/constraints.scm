
;; @ One-Way Simple Constraints

;; This is inspired by a quick reading of a simple micro-constraint
;; system used by the Amulet user interface system. Basically, create
;; a dependency graph, then walk it and re-compute values.

;; For now, this graph only contains one type of constraint
;; system. Really, I need to solve many constraints at the same
;; time. I might need to label the nodes and edges differently in that
;; case.

;; For now, let's assume a graph is somehow created with the following
;; signature:
;;   node :: int * [function, value, boolean]
;;   edge :: int * int * [value, parameter#]

;; The node contains a one-way constraint function and a last-computed
;; value. Also, the boolean tag says whether or not it is
;; out-of-date. The edge contains the previous computed "in" value and
;; the parameter position it should go in the node's function.

;; For now, let's ignore the edge's value.

(define-structure (Constraint function value valid?))

(define ord (g:new-order = <))

(define (compute-node-value node graph)
  ;; g is the graph where all incoming edges have valid data. 
  (let* ((g (g:predecessors (lambda (k v g)
			      (if (Constraint-valid? (g:Context-label (g:get-node ord k g)))
				  g
				  (compute-node-value k g))) ; recursively compute the incoming node
			    graph (g:get-node ord node graph)))
	 (c (g:get-node ord node g))
	 (l (list-sort (lambda (x y) (< (cdr x) (cdr y))) (m:elements (g:Context-preds c))))
	 ;; now just compute the value for this node.
	 (v (apply (Constraint-function (g:Context-label c))
		   (map (lambda (e) 
			  (Constraint-value (g:Context-label (g:get-node ord (car e) g)))) 
			l))))
    (g:update-Context-label ord node
			  (make-Constraint (Constraint-function (g:Context-label c)) v #t) g)))


;; When a node is modified, mark all forward nodes invalid.
(define (perturb-constraint ord node graph)
  (let ((l (depth-first-search ord graph node)))
    (fold (lambda (n g)
	    (let ((c (g:Context-label (g:get-node ord node graph))))
	      (g:update-Context-label ord n
				    (make-Constraint (Constraint-function c) (Constraint-value c) #f)
				    g)))
	  graph l)))



