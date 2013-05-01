;; ,open floatnums
;; ,open srfi-26 srfi-8 srfi-1 srfi-11

(define pi 3.14159265358979323846)

;; for some reason, scheme48 doesn't have make-polar!
(define (make-polar m a) (make-rectangular (* m (cos a)) (* m (sin a))))

(define (make-nth-root len) (make-polar 1 (/ (* 2 pi) len)))


;; i+t = ((index . temp) ...)

(define (print . args) (for-each display args) (newline))

(define *count* 0)
(define (new-temp i)
  (set! *count* (+ *count* 1))
  (string->symbol (string-append "t" (number->string *count*))))
  

(define (unshuffle lst)
  (letrec ((odd (lambda (lst) 
		  (if (null? lst)
		      (values '() '())
		      (receive (evens odds) (even (cdr lst))
			       (values evens (cons (car lst) odds))))))
	   (even (lambda (lst) 
		   (if (null? lst)
		       (values '() '())
		       (receive (evens odds) (odd (cdr lst))
				(values (cons (car lst) evens) odds))))))
    (if (even? (length lst)) (even lst) (odd lst))))


(define (comp w yk ykn y0 y1 j stmts k)
  `(let ((,yk (+ ,y0 (* ,(w j) ,y1))))
     (let ((,ykn (- ,y0 (* ,(w j) ,y1))))
       ,@stmts)))


(define (merge temps even odd k)
  (let ((k (length even))
	(make-w (lambda (k) `(unity-root ,(length temps) ,k))))
    (receive (lhs0 lhs1) (split-at temps k)
	     (fold-right (cut comp make-w <> <> <> <> <> <>) '() lhs0 lhs1 even odd (iota k)))))

(define (merge temps even odd k)
  (let* ((n (length temps))
	 (j (/ n 2)))
    (let-values (((half1 half2) (split-at temps j)))
      (let loop ((half1 half1) (half2 half2)
		 (even even) (odd odd)
		 (i 0))
	(if (= i j)
	    (k temps)
	    `(let ((,(car half1) (+ ,(car even) (* (unity-root ,n ,i) ,(car odd)))))
	       (let ((,(car half2) (- ,(car even) (* (unity-root ,n ,i) ,(car odd)))))
		 ,(loop (cdr half1) (cdr half2) (cdr even) (cdr odd) (+ i 1)))))))))


(define (gen indices k)
  (let ((temps (map new-temp indices)))
    (if (= (length indices) 1)
	`(let ((,(car temps) (input ,(car indices))))
	   ,(k temps))
	(receive (even odd) (unshuffle indices)
		 (gen even (lambda (etmps) 
			     (gen odd (lambda (otmps)
					(merge temps etmps otmps k)))))))))

;		 (let*-values (((even-temps even-code) (gen even))
;			       ((odd-temps odd-code) (gen odd)))
;			      (values temps
;				      (append even-code 
;					      odd-code 
;					      (merge temps even-temps odd-temps))))))))

(define (fftgen n)
  (set! *count* 0)
  (let ((indices (iota n)))
    (gen indices
	 (lambda (temps)
	   `(begin
	      ,@(map (lambda (i t) `(output ,i ,t)) indices temps))))))


(define unity-root-simplification
  (match-lambda
   ;; (unity-root N 0) ==> 1
   (($ App ($ Var 'unity-root) (N ($ Value 0)))
    (make-Value 1 '()))

   (($ App ($ Var 'unity-root) (($ Value 4) ($ Value 1)))
    (make-Value 0+1i '()))

   (($ App ($ Var 'unity-root) (($ Value 2) ($ Value 1)))
    (make-Value -1 '()))

   ;; Halving lemma (p. 785 in CLR's intro to algorithms)
   ;; (unity-root N (K + N/2)) ==> (- (unity-root N K))
   (($ Let V ($ App ($ Var 'unity-root) ((and ($ Value (? even? Root)) N) ($ Value K))) B) (=> fail)
    (let ((half (/ Root 2))
	  (T (gensym-Var)))
      (if (<= K half) (fail)
	  (make-Let T (make-App (make-Var 'unity-root '())
				(list (make-Value N '()) (make-Value (- K half) '()))
				'())
		    (make-Let V (make-App (make-Var '- '()) (list T) '()) B '())))))

   ;; Cancellation lemma (p. 785 in CLR's intro to algorithms)
   ;; (unity-root (* d N) (* d K)) ==> (unity-root N K)
   (($ App ($ Var 'unity-root) (($ Value N) ($ Value (and (not 0) K)))) (=> fail)
    (let ((d (gcd N K)))
      (if (> d 1)
	  (make-App (make-Var 'unity-root '())
		    (list (make-Value (/ N d) '()) (make-Value (/ K d) '()))
		    '())
	  (fail))))))

