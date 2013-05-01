
(define (print tree) (display tree) (newline) tree)
(define manydownup (strategy (d u) (rec x (choice (seq d (all (try x)) (try u))
						  (seq (some x) (try u))
						  u))))


(define constant-fold/+ 
  (match-lambda 
   (($ App ($ Var '+) (arg)) arg)
   (($ App (and f ($ Var '+)) args) (=> fail)
    (let-values (((constants variables) (l:partition Value? args)))
		(if (> (length constants) 1)
		    (make-App f (cons (make-Value (apply + (map Value-data constants)) '()) variables) '())
		    (fail))))))

(define constant-fold/+ 
  (match-ANF
   ((+ arg) arg)
   ((+ args ...) (=> fail) 
    (let-values (((constants variables) (l:partition Value? args)))
		(if (> (length constants) 1)
		    (make-App f (cons (make-Value (apply + (map Value-data constants)) '()) variables) '())
		    (fail))))))


(define (side-effected? exit)
  (lambda (var tree)
  (match-lambda 
   (($ Set ($ Var name) _) (=> fail)
    (

(define (where pred)
  (call/cc
   (lambda (exit)
     (pred exit))))

(define constant-propagation
  (match-lambda 
   (($ Let ($ Var name) (? Value? C) B) (=> fail)
    (let* ((name? (lambda (s) (equal? name s)))
	   (propagate (try (manytd (match-lambda 
				    (($ Set ($ Var (? name?))) (fail))
				    (($ Var (? name?)) C))))))
      (propagate B)))))
