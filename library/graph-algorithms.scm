
(define l:reverse reverse)		; should I rename my reverse proc? or capture the R5RS name?

;; reverse all edges in a graph
(define (reverse order graph)
  (let ((swap-edges 
	 (lambda (ctx g)
	   (put-context order 
			(make-context (context-succs ctx)
				      (context-node ctx)
				      (context-label ctx)
				      (context-preds ctx))
			g))))
    (fold swap-edges (empty) graph)))


(define (depth-first-search ord graph . start)
  (let ((visit (if (null? start) (nodes graph) start)))
    (search ord append visit graph)))

(define (breadth-first-search ord graph . start)
  (let ((visit (if (null? start) (nodes graph) start)))
    (search ord (lambda (x y) (append y x)) visit graph)))

;; TODO: ensure this gives correct answer; compare with test oracle
(define (topological-sort ord visit graph)
  (l:reverse (l:append-map postorder (spanning-forest ord visit graph))))

(define (spanning-forest ord visit graph)
  (define (forest v g)
    (cond
     ((null? v) (values '() g))
     ((get-node ord (car v) g)
      => (lambda (ctx)
	   (receive (f1 g1) (forest (succ-edges ctx)
				    (rem-node ord (car v) g))
		    (receive (f2 g2) (forest (cdr v) g1)
			     (values (cons (cons (car v) f1) f2) g2)))))
     (else (forest (cdr v) g))))
  (receive (f g) (forest visit graph) f))


;; @@ Auxillary 

(define (search ord organize visit graph)
  (cond 
   ((null? visit) '())
   ((equal? graph (empty)) '())
   ((get-node ord (car visit) graph)
    => (lambda (ctx) 
	 (cons (car visit)
	       (search ord organize (organize (succ-edges ctx) (cdr visit))
		       (rem-node ord (car visit) graph)))))
   (else (search ord organize (cdr visit) graph))))

;; type tree = int * tree list
(define (postorder tree)
  (if (null? tree)
      '()
      (l:fold append (list (car tree)) (map postorder (cdr tree)))))

(define (succ-edges ctx)
  (successors (lambda (n lbl lst) (cons n lst)) '() ctx))


