

;; TODO: Still not right! make-graph and to-dot need to be fixed
;; TODO: new-context and new-graph need order structures for the edge maps too.


;; type ('e) edge = ('e,int)
;; type ('node,'edge) context = (int,'edge edge) map * int * 'node * (int,'edge edge) map
;; type ('node,'edge) graph = (int,('node,'edge) context) map

;; @ Functional Graph implementation

;; don't modify a possibly shared context!
(define-structure (Context preds node label succs))

(define (new-context node label) (make-Context (empty) node label (empty)))

(define (copy-context ctx) 
  (make-Context (Context-preds ctx)
		(Context-node ctx)
		(Context-label ctx)
		(Context-succs ctx)))

;; @@@ Implementation of collection interface

(define new-order m:new-order)

(define new m:new)
(define size m:size)


;; node * graph -> context
(define (get-node node graph) (m:member node graph))

;; node * label * graph -> graph
(define (put-node node label graph)
  (m:insert node (new-Context node label) graph))

;; node * graph -> graph 
(define (rem-node node graph)
  (let ((ctx (get-node node graph)))
    (m:remove node			; remove all the edges, too!
	      (m:fold (lambda (k v g) (rem-edge node k g))
		      (m:fold (lambda (k v g) (rem-edge k node g))
			      graph 
			      (Context-preds ctx))
		      (Context-succs ctx)))))
    

;; Manage edges

;; edge :: (node, ?) map

;; node * node * graph -> edge
(define (get-edge fromNode toNode graph)
  (m:member toNode (Context-succs (get-node fromNode graph))))

;; node * node * label * graph -> graph 
(define (put-edge fromNode toNode label graph)
  (let ((fromContext (copy-context (get-node fromNode graph)))
	(toContext (copy-context (get-node toNode graph)))
	(minusGraph (m:remove fromNode (m:remove toNode graph))))
    (set-Context-succs! fromContext (m:insert toNode label (Context-succs fromContext)))
    (set-Context-preds! toContext (m:insert fromNode label (Context-preds toContext)))
    (m:insert fromNode fromContext
	      (m:insert toNode toContext minusgraph))))

;; node * node * graph -> graph 
(define (rem-edge fromNode toNode graph)
  (let ((fromContext (copy-context (get-node fromNode graph)))
	(toContext (copy-context (get-node toNode graph)))
	(minusGraph (m:remove fromNode (m:remove toNode graph))))
    (set-Context-succs! fromContext (m:remove toNode (Context-succs fromContext)))
    (set-Context-preds! toContext (m:remove fromNode (Context-preds toContext)))
    (m:insert fromNode fromContext
	      (m:insert toNode toContext minusgraph))))


;; special purpose procedures. These shouldn't really be here.

;; context * graph -> graph
(define (put-context context graph)
  (m:insert (Context-node context) context graph))

;; order * node * graph -> graph
(define (rem-context node graph)
  (m:remove node graph))


;; A quick way to update a context label
(define (update-Context-label node label graph)
  (let ((c (copy-context (get-node node graph))))
    (set-Context-label! c label)
    (put-context c (rem-context node graph))))


;; (node * label * init -> init) * init * context -> init 
(define (successors f init context)
  (m:fold f init (context-succs context)))

(define (predecessors f init context)
  (m:fold f init (context-preds context)))


;; (context... init -> init) * init * graphs... -> init
(define fold m:fold)

(define nodes m:keys)


;; order * [(node label)] * [(from to label)] -> graph
(define (make-graph nodes edges)
  (l:fold (lambda (edges g) (put-edge (l:first edges) (l:second edges) (l:third edges) g))
	  (l:fold (lambda (node g) (put-node (l:first node) (l:second node) g))
		  (empty)
		  nodes)
	  edges))


(define (to-dot name graph . output-port)
  (let ((port (if (null? output-port) (current-output-port) (car output-port))))
    (letrec ((print (lambda args 
		      (let loop ((i 0))
			(if (< i indentation)
			    (begin (display " " port) (loop (+ i 1)))))
		      (for-each (lambda (arg) (display arg port)) args) (newline port)))
	     (make-label
	      (lambda (lbl)
		(string-append " [ label = \"" lbl "\" ]")))
	     (indentation 0)
	     (indent! (lambda () (set! indentation (+ indentation 3))))
	     (outdent! (lambda () (set! indentation (- indentation 3)))) ; made up a new word!
	     )
      (print "digraph " name " {")
      (indent!)
      (fold (lambda (ctx none)
	      (print (context-node ctx) " [ label = \"" (context-label ctx) "\" ]"  ";")
	      (indent!)
	      (m:fold (lambda (entry none)
			(print (context-node ctx) " -> " (m:entry-key entry) 
			       " [ label = \"" (m:entry-value entry) "\" ]" " ;"))
		      '()
		      (context-succs ctx))
	      (outdent!))
	    '()
	    graph)
      (outdent!)
      (print "}"))))


