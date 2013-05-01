
;; @ Implementation of collection interface 

(define (new order) 
  (make-collection order '()))

(define (size set) 
  (length (collection-value set)))

(define (member elem set) 
  (and-let* ((lst (l:member elem (collection-value set) (order-= (collection-order set))))) 
	    (car lst)))

(define (remove elem set) 
  (let ((s (collection-value set))
	(o (collection-order set)))
    (make-collection o
		    (l:remove (lambda (x) ((order-= o) elem x)) s))))

(define (insert elem set) 
  (let ((set (if (member elem set) (remove elem set) set)))
    (make-collection (collection-order set)
		    (cons elem (collection-value set)))))

(define (fold f i set)
  (l:fold f i (collection-value set)))

(define (elements set) 
  (collection-value set))			; because s is implemented as a list

;; @ Implementation of set interface 

;; these assume all sets use the same ordering rules

(define (union set . sets)		
  (if (null? sets) 
      set
      (let ((o (collection-order set))
	    (set (collection-value set))
	    (sets (map collection-value sets)))
	(make-collection o (apply l:lset-union (order-= o) set sets)))))

(define (intersection set . sets) 
  (if (null? sets) 
      set
      (let ((o (collection-order set))
	    (set (collection-value set))
	    (sets (map collection-value sets)))
	(make-collection o (apply l:lset-intersection (order-= o) set sets)))))

(define (difference set . sets)
  (if (null? sets) 
      set
      (let ((o (collection-order set))
	    (set (collection-value set))
	    (sets (map collection-value sets)))
	(make-collection o (apply l:lset-difference (order-= o) set sets)))))
