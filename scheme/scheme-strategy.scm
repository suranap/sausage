
;; @ Core Strategy for Scheme

;; To use the rewrite strategies with the Scheme AST (parse.scm), the
;; core rules must be implemented. These are ALL, SOME, and ONE.

;; @ Helpers

(define-syntax receive
      (syntax-rules ()
	((receive formals expression body ...)
	 (call-with-values (lambda () expression)
			   (lambda formals body ...)))))

(define (and-map f lst)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((l lst))
       (cond 
	((null? l) '())
	((f (car l)) => (lambda (v) (cons v (recur (cdr l)))))
	(else (abort #f)))))))

(define (or-map f lst)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((l lst) (flag #f))
       (cond
	((and (null? l) flag) '())
	((and (null? l) (not flag)) (abort lst))
	((f (car l)) => (lambda (v) (cons v (recur (cdr l) #t))))
	(else (cons (car l) (recur (cdr l) flag))))))))

(define (once-map f lst)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((l lst))
       (cond
	((null? l) (abort #f))
	((f (car l)) => (lambda (v) (cons v (cdr l))))
	(else (cons (car l) (recur (cdr l)))))))))


;; @ Core rewrite rules

;; @@ ALL

(define (all s)
  (match-lambda 
   (($ Define V B a)  (and-let* ((b1 (s B))) (make-Define V b1 a)))
   (($ Set V V2 a)    (and-let* ((v1 (s V2))) (make-Set V v1 a)))
   (($ Lambda P* B a) (and-let* ((b1 (s B))) (make-Lambda P* b1 a)))
   (($ Begin B E a)   (and-let* ((b1 (s B)) (e1 (s E))) (make-Begin b1 e1 a)))
   (($ Let V E B a)   (and-let* ((e1 (s E)) (b1 (s B))) (make-Let V e1 b1 a)))
   (($ Rec Bs B a)    (receive (Vs Es) (unzip2 Bs)
			       (and-let* ((es1 (and-map s Es)) (b1 (s B)))
					 (make-Rec (zip Vs es1) b1 a))))
   (($ If C T E a)    (and-let* ((c1 (s C)) (t1 (s T)) (e1 (s E))) (make-If c1 t1 e1 a)))
   (($ App F A* a)    (and-let* ((f1 (s F)) (a1 (and-map s A*))) (make-App f1 a1 a)))
   ((? Var? e)        e)
   ((? Value? e)      e)))


;; @@ SOME

(define (some s)
  (match-lambda 
   (($ Define V B a)  (and-let* ((b1 (s B))) (make-Define V b1 a)))
   (($ Set V V2 a)    (and-let* ((v1 (s V2))) (make-Set V v1 a)))
   (($ Lambda P* B a) (and-let* ((b1 (s B))) (make-Lambda P* b1 a)))
   (($ Begin B E a)   (and-let* ((l (list B E)) (l1 (or-map s l)) ((not (eqv? l l1))))
				(make-Begin (first l1) (second l1) a)))
   (($ Let V E B a)   (and-let* ((l (list E B)) (l1 (or-map s l)) ((not (eqv? l l1)))) 
				(make-Let V (first l1) (second l1) a)))
   (($ Rec Bs B a)    (receive (Vs Es) (unzip2 Bs)
			       (and-let* ((l (cons B Es)) (l1 (or-map s l)) ((not (eqv? l l1))))
					 (make-Rec (zip Vs (cdr l1)) (first l1) a))))
   (($ If C T E a)    (and-let* ((l (list C T E)) (l1 (or-map s l)) ((not (eqv? l l1))))
				(make-If (first l1) (second l1) (third l1) a)))
   (($ App F A* a)    (and-let* ((l (cons F A*)) (l1 (or-map s l)) ((not (eqv? l l1))))
				(make-App (car l1) (cdr l1) a)))
   ((? Var? e)        #f)
   ((? Value? e)      #f)))


;; @@ ONE

(define (one s)
  (match-lambda 
   (($ Define V B a)  (and-let* ((b1 (s B))) (make-Define V b1 a))) 
   (($ Set V V2 a)    (and-let* ((v1 (s V2))) (make-Set V v1 a)))
   (($ Lambda P* B a) (and-let* ((b1 (s B))) (make-Lambda P* b1 a)))
   (($ Begin B E a)   (and-let* ((l (list E B)) (l1 (once-map s l)) (not (eqv? l l1)))
				(make-Begin (first l1) (second l1) a)))
   (($ Let V E B a)   (and-let* ((l (list E B)) (l1 (once-map s l)) ((not (eqv? l l1))))
				(make-Let V (first l1) (second l1) a)))
   (($ Rec Bs B a)    (receive (Vs Es) (unzip2 Bs)
			       (and-let* ((l (cons B Es)) (l1 (once-map s l)) ((not (eqv? l l1))))
					 (make-Rec (zip Vs (cdr l1)) (first l1) a))))
   (($ If C T E a)    (and-let* ((l (list C T E)) (l1 (once-map s l)) ((not (eqv? l l1))))
				(make-If (first l1) (second l1) (third l1) a)))
   (($ App F A* a)    (and-let* ((l (cons F A*))  (l1 (once-map s l)) ((not (eqv? l l1))))
				(make-App (car l1) (cdr l1) a)))
   ((? Var? e)        #f)
   ((? Value? e)      #f)))


