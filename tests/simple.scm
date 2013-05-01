
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(define (deforest f g lst)
  (map f (map g lst)))

(define polyvariant
  (let ((f (lambda (x) x))
	(g (lambda (h y z) (h y) (h x))))
    (g f 1 2)
    (g f 3 4)))


(define map
  (lambda (f l)
    (reverse!
     (let loop ((l l))
       (if (null? l) '()
	   (cons (f (car l)) (loop (cdr l))))))))

