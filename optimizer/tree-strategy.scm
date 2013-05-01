
;; @ Strategy for Multi-Way Trees

;; multiway tree: A tree with any number of children for each node.
;;   type tree = () | (node children ...)


;; @ Auxillary

;; apply function to all lists unless one fails
(define (and-map f lst . lsts)
  (apply fold-right 
	 (lambda args
	   (let ((e (drop-right args 1)) (i (last args)))
	     (and-let* (i (a (apply f e))) (cons a i))))
	 '()
	 lst
	 lsts))


;; apply predicate to all members of lst. If f fails, insert original element.
(define (or-map f lst)
  (fold-right (lambda (e i) (cons (or (f e) e) i)) '() lst))


;; apply predicate to lst until one member succeeds.
(define (once-map f lst)
  (if (null? lst) 
      #f
      (let recur ((h (car lst)) (r (cdr lst)))
	(cond
	 ((f h) => (lambda (a) (cons a r)))
	 ((null? r) #f)
	 (else
	  (let ((a (recur (car r) (cdr r))))
	    (if a (cons h a) #f)))))))


;; @ Low-Level Strategies

;; @@ ALL 

;; Apply a strategy to ALL the children of the tree. If the tree is
;; empty, return false. I think this matches the definition of for-all
;; in set theory.

(define (all s)
  (lambda (tree)
    (cond
     ((null? tree) tree)
;     ((pair? tree) (and-let* ((children (and-map s (cdr tree))))
;			     (cons (car tree) children))))))
     ((pair? tree) (and-map s tree))
     (else tree))))


;; @@ SOME

;; Apply a strategy to all the children of the tree. If the strategy
;; fails for any of the children, use the original child instead. But
;; it must succeed for at least one child.

(define (some s)
  (lambda (tree)
    (cond
     ((null? tree) #f)
;     ((pair? tree) (let ((ch (or-map s (cdr tree))))
;		     (if (and-map eqv? ch (cdr tree))
;			 #f			; no child changed, fail
;			 (cons (car tree) ch))))
     ((pair? tree) (let ((ch (or-map s tree)))
		     (if (and-map eqv? ch tree)
			 #f			; no child changed, fail
			 ch)))
		     
     (else #f))))


;; @@ ONE

(define (one s)
  (lambda (tree)
    (cond
     ((null? tree) #f)
;     ((pair? tree) (and-let* ((head (car tree)) (rest (once-map s (cdr tree))))
;			     (cons head rest)))
     ((pair? tree) (once-map s tree))
     (else #f))))


