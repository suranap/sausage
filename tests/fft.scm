;; ,open floatnums

;; for some reason, scheme48 doesn't have make-polar!
(define pi 3.14159265358979323846)

(define (make-polar m a) (make-rectangular (* m (cos a)) (* m (sin a))))

(define (make-nth-root len) (make-polar 1 (/ (* 2 pi) len)))

(define (unity-root n k) (expt (make-nth-root n) k))

(define (my-fft sequence)
  (let ((unshuffle (lambda (seq)
		     (let loop ((rest seq)
				(evens '())
				(odds '()))
		       (if (null? rest)
			   (cons (reverse evens) (reverse odds))
			   (loop (cddr rest)
				 (cons (car rest) evens)
				 (cons (cadr rest) odds)))))))
    (let ((len (length sequence)))
      (if (= len 1)
	  sequence
	  (let ((nth-root (make-nth-root len))
					; principal len-th root of unity
		(half-len (quotient len 2))
		(packs (unshuffle sequence)))
	    (let loop ((step 0)
		       (root 1)
		       (evens (my-fft (car packs)))
		       (odds (my-fft (cdr packs)))
		       (front '())
		       (rear '()))
	      (if (= step half-len)
		  (append (reverse front) (reverse rear))
		  (loop (+ step 1)
			(* root nth-root)
			(cdr evens)
			(cdr odds)
			(cons (+ (car evens) (* root (car odds)))
			      front)
			(cons (- (car evens) (* root (car odds)))
			      rear)))))))))


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


(define (comp w yk ykn y0 y1 k stmts)
  (cons* `(set! ,yk (+ ,y0 (* ,(w k) ,y1)))
	 `(set! ,ykn (- ,y0 (* ,(w k) ,y1)))
	 stmts))

(define (merge temps even odd)
  (let ((k (length even))
	(make-w (lambda (k) `(unity-root ,(length temps) ,k))))
    (receive (lhs0 lhs1) (split-at temps k)
	     (fold-right (cut comp make-w <> <> <> <> <> <>) '() lhs0 lhs1 even odd (iota k)))))

(define (gen indices)
  (print (length indices))
  (let ((temps (map new-temp indices)))
    (if (= (length indices) 1)
	(values temps
		`((set! ,(car temps) (input ,(car indices))))
		)
	(receive (even odd) (unshuffle indices)
		 (let*-values (((even-temps even-code) (gen even))
			       ((odd-temps odd-code) (gen odd)))
			      (values temps
				      (append even-code 
					      odd-code 
					      (merge temps even-temps odd-temps))))))))
(define (fftgen n)
  (set! *count* 0)
  (gen (iota n)))



(define (fft-data)
  (list->vector 
   (let ((x (/ (* 2 pi 5) 16)))
     (map (lambda (a)
	    (make-rectangular (cos (* x a)) (- (sin (* x a)))))
	  (l:iota 16)))))