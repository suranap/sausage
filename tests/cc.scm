
;; define a tree iterator

(define call/cc call-with-current-continuation)
(define (print . lst) (for-each display lst) (newline))

(define (value val k) (list 'value val k))
(define (value? v) (and (pair? v) (eqv? 'value (car v))))
(define value-v cadr)
(define value-k caddr)


;; updates all nodes
(define (test-cc iter tree)
  (if (pair? tree) 
      (list (test-cc iter (car tree))
	    (test-cc iter (cadr tree)))
      (+ tree 1)))

;; "yields" old val and cc to iterator
(define (test-cc1 iter tree)
  (if (pair? tree) 
      (list (test-cc1 iter (car tree))
	    (test-cc1 iter (cadr tree)))
      (call/cc (lambda (k) (iter (value tree k))))))


;; 
(define (test-cc2 f tree)		; -> (values tree k)
  (if (pair? tree)
      (call-with-values (lambda () (test-cc2 f (car tree)))
	(lambda (tree1 k)
	  (if k
	      (values (cons tree1 (cdr tree)) k)
	      (call-with-values (lambda () (test-cc2 f (cadr tree)))
		(lambda (tree2 k)
		  (values (list tree1 tree2) k))))))
      (let ((leaf (f tree)))
	(if leaf
	    (values leaf (call/cc (lambda (k) k)))
	    (values tree #f)))))

(define (list-cc f lst)
  (cond 
   ((null? lst) (values lst #f))
   ((f (car lst)) 
    => (lambda (a)
	 (call/cc (lambda (k)
		    (values (cons a (cdr lst))
			    (lambda () 
			      (k (call-with-values (lambda () (list-cc f (cdr lst)))
				   (lambda (lst1 k1)
				     (values (cons a lst1) k1))))))))))
   (else
    (call-with-values (lambda () (list-cc f (cdr lst)))
      (lambda (lst1 k1)
	(values (cons (car lst) lst1) k1))))))


(define (iterator tree)
  (let ((val (call/cc 
	      (lambda (k)
		(test-cc k tree)))))
    (if (value? val)
	(begin (print "iterator: " (value-v val))
	       ((value-k val) (+ (value-v val) 1)))
	val)))

;; algorithm
;;   - go to leaf node
;;   - update node
;;   - return tree and cc
;;   - if user wants to continue, cc 


;; do it on a flat list
(define (map-cc f lst)
  (if (pair? lst)
      (let* ((h (f (car lst)))
	     (t (call/cc (lambda (k)
			   (value (cdr lst)
				  (lambda ()
				    (k (map-cc f (cdr lst)))))))))
	(value (cons h (value-v t))
	       (value-k t)))
      (value lst #f)))


;; type tree = () | (head children ...)
;; for each child in children, (f child)
(define (iter-map return f lst)
  (if (null? lst) 
      lst
      (let recur ((h (car lst)) (r (cdr lst)))
	(cond
	 ((f h) => (lambda (a) 