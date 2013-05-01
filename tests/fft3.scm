;; ,open floatnums

(define (print . args) (for-each display args) (newline))

;; for some reason, scheme48 doesn't have make-polar!
(define pi 3.14159265358979323846)

(define (make-polar m a) (make-rectangular (* m (cos a)) (* m (sin a))))

(define (make-nth-root len) (make-polar 1 (/ (* 2 pi) len)))

(define (unity-root n k) (expt (make-nth-root n) k))


(define gensym-global 0)
(define (gensym t)
  (set! gensym-global (+ 1 gensym-global))
  (string->symbol (string-append "tmp" (number->string gensym-global))))

(define (evens lst)
  (let next ((lst lst)
	     (even #t))
    (if (null? lst) lst
	(if even 
	    (cons (car lst) (next (cdr lst) #f))
	    (next (cdr lst) #t)))))

(define (odds lst)
  (let next ((lst lst)
	     (odd #f))
    (if (null? lst) lst
	(if odd
	    (cons (car lst) (next (cdr lst) #f))
	    (next (cdr lst) #t)))))


(define fft-rule
  (match-lambda
   (('fft n)
    (let* ((indices (l:iota n))
	   (inputs (map gensym indices))
	   (outputs (map gensym indices)))
      (let ((input-code 
	     (map (lambda (v i)
		    `(,v (vector-ref a ,i)))
		  inputs indices))
	    (output-code
	     (map (lambda (v i)
		    `(vector-set! a ,i ,v))
		  outputs indices)))
	`(lambda (a)
	   (let ,input-code
	     (call-with-values (lambda () (recursive-fft ,@inputs))
	       (lambda ,outputs
		 ,@output-code)))))))))

(define recursive-fft
  (match-lambda
   (('recursive-fft input) `(values ,input))
   (('recursive-fft inputs ...)
    (let ((n (length inputs))
	  (outputs (map gensym inputs)))
      `(call-with-values (lambda () (recursive-fft ,@(evens inputs)))
	(lambda ,(l:take outputs (/ n 2))
	  (call-with-values (lambda () (recursive-fft ,@(odds inputs)))
	    (lambda ,(l:drop outputs (/ n 2))
	      (merge-fft ,@outputs)))))))))

(define merge-fft
  (match-lambda
   (('merge-fft inputs ...)
    (let* ((n (length inputs))
	   (y0 (l:take inputs (/ n 2)))
	   (y1 (l:drop inputs (/ n 2)))
	   (outputs (map gensym inputs)))
      `(values 
	,@(map (lambda (y0 y1 k) 
		 `(cx+ ,y0 (cx* (unity-root ,n ,k) ,y1))) 
	       y0 y1 (l:iota (/ n 2)))
	,@(map (lambda (y0 y1 k) 
		 `(cx- ,y0 (cx* (unity-root ,n ,k) ,y1))) 
	       y0 y1 (l:iota (/ n 2))))))))

(define unity-root-rule
  (match-lambda
   (('unity-root N 0) 1)
   (('unity-root 2 1) -1)
   (('unity-root 4 1) 0+1i)

   ;; cancellation lemma 
   (('unity-root N K) (=> fail)
    (if (and (even? N) (> K (/ N 2)))
	`(negate (unity-root ,N ,(- K (/ N 2))))
	(fail)))

   ;; halving lemma
   (('unity-root N K) (=> fail)
    (if (zero? K) (fail)
	(let ((d (gcd N K)))
	  (if (> d 1)
	      `(unity-root ,(/ N d) ,(/ K d))
	      (fail)))))

   ;; compute unity root
   (('unity-root N K) (unity-root N K))))

(define simplify-math-rule
  (match-lambda 
   (((or '* 'cx*) (? number? X) (? number? Y)) (* X Y))
   (((or '+ 'cx+) (? number? X) (? number? Y)) (+ X Y))
   (((or '* 'cx*) 1 N) N)
   (((or '* 'cx*) N 1) N)
   (((or '* 'cx*) 0 N) 0)
   (((or '* 'cx*) N 0) 0)
   (((or '+ 'cx+) 0 N) N)
   (((or '+ 'cx+) N 0) N)
   (('- 0 N) `(negate ,N))))


(define values-rule
  (match-lambda
;   (('call-with-values ('lambda () ('values args ...)) ('lambda (params ...) body))
;    `(let ,(l:zip params args) ,body))
   (('call-with-values ('lambda () E) ('lambda (params ...) B ...))
    ((somebu
      (match-lambda 
       (('values args ...) `(let ,(l:zip params args) ,@B))))
     E))))

(define useless-variable-rule
  (match-lambda
   (('let ((V (? symbol? E))) B) 
    (let ((V? (cut equal? <> V)))
      ((manybu (match-lambda ((? V?) E))) B)))))

(define simplify-let
  (match-lambda
   (((or 'let 'let*) (B1 B2 Bs ...) Es ...) `(let (,B1) (let (,B2 ,@Bs) ,@Es)))))

(define fft-stage
  (strategy (reduce (choice fft-rule recursive-fft merge-fft unity-root-rule values-rule  simplify-let simplify-math-rule lift-exprs))))

(define fft-stage2
  (strategy (repeat (choice (manybu fft-rule) (manybu recursive-fft) (manybu merge-fft) (manybu unity-root-rule) (manybu values-rule) (manybu simplify-let) (manybu simplify-math-rule) (manybu lift-exprs)))))

(define fft-library 
  (strategy (reduce (choice fft-rule recursive-fft merge-fft unity-root-rule values-rule useless-variable-rule simplify-let simplify-math-rule lift-exprs cse))))
    

(define collapse-let
  (match-lambda
   (((or 'let 'let*) (B1s ...) ((or 'let 'let*) (B2s ...) Es ...))
    `(let* (,@B1s ,@B2s) ,@Es))))

(define lift-exprs
  (match-lambda
   (('let ((V ((and (or '- '+ '* 'cx 'cx+ 'cx*) F) (? pair? E1) E2))) B ...)
    (let ((T (gensym 0)))
      `(let ((,T ,E1)) (let ((,V (,F ,T ,E2))) ,@B))))
   (('let ((V ((and (or '- '+ '* 'cx 'cx+ 'cx*) F) E1 (? pair? E2)))) B ...)
    (let ((T (gensym 0)))
      `(let ((,T ,E2)) (let ((,V (,F ,E1 ,T))) ,@B))))
   (('let ((V ('negate (? pair? E)))) B ...)
    (let ((T (gensym 0)))
      `(let ((,T ,E)) (let ((,V (negate ,T))) ,@B))))))


(define cse
  (match-lambda 
   (('let ((V (? pair? E))) B)
    (let* ((E? (cut equal? <> E))
	   (newB ((manybu (match-lambda ((? E?) V))) B)))
      (and newB 
	   `(let ((,V ,E)) ,newB))))))

(define cx-ops
  (match-lambda 
   (('cx+ (? number? N) V) 
    `(cx (+ ,(real-part N) (real-part ,V)) (+ ,(imag-part N) (real-part ,V))))
   (('cx+ V (? number? N))
    `(cx+ ,N ,V))
   (('cx* (? number? N) V)
    `(cx (- (* ,(real-part N) (real-part ,V)) (* ,(imag-part N) (imag-part ,V)))
	 (- (* (+ ,(real-part N) ,(imag-part N)) (+ (real-part ,V) (imag-part ,V)))
	    (- (* ,(real-part N) (real-part ,V)) (* ,(imag-part N) (imag-part ,V))))))
   (('cx* V (? number? N))
    `(cx* ,N ,V))))





;;; ********************************************
;;; ********************************************
;;; ********************************************

(define (my-fft-rule n)
  (let* ((indices (l:iota n))
	 (inputs (map gensym indices))
	 (outputs (map gensym indices)))
    (let ((input-code 
	   (map (lambda (v i)
		  `(,v (vector-ref a ,i)))
		inputs indices))
	  (output-code
	   (map (lambda (v i)
		  `(vector-set! a ,i ,v))
		outputs indices)))
      `(lambda (a)
	 (let ,input-code
	   (call-with-values (lambda () ,(apply my-recursive-fft inputs))
	     (lambda ,outputs
	       ,@output-code)))))))

(define (my-recursive-fft . inputs)
  (match inputs
   ((input) `(values ,input))
   ((inputs ...)
    (let ((n (length inputs))
	  (outputs (map gensym inputs)))
      `(call-with-values (lambda () ,(apply my-recursive-fft (evens inputs)))
	(lambda ,(l:take outputs (/ n 2))
	  (call-with-values (lambda () ,(apply my-recursive-fft (odds inputs)))
	    (lambda ,(l:drop outputs (/ n 2))
	      ,(apply my-merge-fft outputs)))))))))

(define (my-merge-fft . inputs)
  (let* ((n (length inputs))
	(y0 (l:take inputs (/ n 2)))
	(y1 (l:drop inputs (/ n 2)))
	(outputs (map gensym inputs)))
    `(values 
      ,@(map (lambda (y0 y1 k) 
	       `(+ ,y0 (* (unity-root ,n ,k) ,y1))) 
	     y0 y1 (l:iota (/ n 2)))
      ,@(map (lambda (y0 y1 k) 
	       `(- ,y0 (* (unity-root ,n ,k) ,y1))) 
	     y0 y1 (l:iota (/ n 2))))))


;; fft 256 = 550.68sec
