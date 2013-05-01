
(define (print . args) (for-each display args) (newline))

(define test
  (match-lambda 
   (e (display e) (newline) e)))


(define (acons k v alst) (cons (cons k v) alst))
(define (acons! k v alst) (set! *vars* (acons k v alst)))


(define (linear? v) (member v *vars*))

(define (linear-types tree)
  (match tree
    (('let (v ('map . _)) _) (set! *vars* (cons v *vars*)) tree)
    (('map f (? linear? l)) `(map! ,f ,l))
    (_ #f)))

(define t1 (normalize-term (macro-expand '(define (deforest f g lst) (map f (map g lst))))))

(define mytd (strategy (s) (rec x (seq (try s) (some x)))))

(define (odd-node? tree)
  (match tree
	 ((n . _) (and (odd? n) tree))
	 (_ #f)))

(define (check ast)
  (match ast
   (($ Define V B a)  (display 'define) (newline) ast)
   (($ Set V V2 a)  (display 'set!) (newline) ast)
   (($ Lambda P+ B a) (display 'lambda) (newline) ast)
   (($ Begin S* a)   (display 'begin) (newline) ast)
   (($ Let V E B a)  (display 'let) (newline) ast)
   (($ If C T E a)   (display 'if) (newline) ast)
   (($ App F A* a)   (display 'app) (newline) ast)
   ((? Var? e)       (display 'var) (newline) ast)
   ((? Value? e)     (display 'value) (newline) ast)))


;; generic attr setter (generates NEW structure)
(define (set-Scheme-attr! node val)
   (($ Define V B a)  (make-Define V B val))
   (($ Set V V2 a)   (make-Set V V2 val))
   (($ Lambda P+ B a) (make-Lambda P+ B val))
   (($ Begin S* a)    (make-Begin S* val))
   (($ Let V E B a)   (make-Let V E B val))
   (($ If C T E a)    (make-If C T E val))
   (($ App F A* a)    (make-App F A* val))
   (($ Var N a)       (make-Var N val))
   (($ Value D a)     (make-Value D val)))


(define (merge ord m1 m2)
  (m:fold (lambda (e m) 
	  (cond 
	   ((m:member ord (m:entry-key e) m) 
	    => (lambda (p) (m:insert ord (m:new-entry (m:entry-key e)
						  (cons (+ (car (m:entry-value e)) (car (m:entry-value p)))
							(+ (cdr (m:entry-value e)) (cdr (m:entry-value p)))))
				   m)))
	   (else (m:insert ord e m))))
	m1 m2))


;; count (use . set) for each variable
(define (use1 ord v) (m:insert ord (m:new-entry (symbol->string v) (cons 1 0)) (m:empty)))
(define (set1 ord v) (m:insert ord (m:new-entry (symbol->string v) (cons 0 1)) (m:empty)))

(define count-def/use
  (match-lambda 
   (($ Define V B a)  
    (make-Define V B (Scheme-attr B)))

   (($ Set (and V ($ Var N a1)) V2 a2) 
    (make-Set V V2 (merge ord (set1 ord N) (Scheme-attr V2))))

   (($ Lambda P+ B a) 
    (make-Lambda P+ B 
		 (merge ord 
		  (l:fold (lambda (n m) (merge ord (set1 ord n) m)) (m:empty) (map Var-name P+))
		  (Scheme-attr B))))

   (($ Begin S* a) 
    (make-Begin S* (m:fold (lambda (m1 m2) (merge ord m1 m2)) (m:empty) (map Scheme-attr S*))))

   (($ Let (and V ($ Var N a1)) E B a) 
    (make-Let V E B (merge ord (merge ord (set1 ord N) (Scheme-attr E)) (Scheme-attr B))))

   (($ If C T E a)
    (make-If C T E (merge ord (Scheme-attr C) (merge ord (Scheme-attr T) (Scheme-attr E)))))

   (($ App F A* a)
    (make-App F A* (m:fold (lambda (m1 m2) (merge ord m1 m2)) (Scheme-attr F) (map Scheme-attr A*))))

   (($ Var N a) 
    (make-Var N (use1 ord N)))

   ((? Value? e) e)			; leave it empty
   ))



(define (collect-linear-type tree)
  (match tree
	 (($ Define V B a) (set! non-linear-type (cons (Var-name V) non-linear-type)) tree) 
	 (($ Set V V2 a) (set! non-linear-type (cons (Var-name V) non-linear-type)) tree) 
	 (($ Lambda P+ B a) (set! non-linear-type (append (map Var-name P+) non-linear-type)) tree)
	 (($ Begin S* a)    tree)
	 (($ Let V ($ Var N _) B a) (if (member N non-linear-type) 
					(set! non-linear-type (cons (Var-name V) non-linear-type)))
	  tree)
	 (($ Let V E B a)  tree)
	 (($ If C T E a)   tree)
	 (($ App F A* a)   tree)
	 ((? Var?) tree)
	 ((? Value?) tree)))


(define transform-linear-function
  (match-lambda 
   (($ App ($ Var 'map a1) (F L) a2) (and-let* (((not (l:member (Var-name L) non-linear-type)))
					      (e (m:member ord (symbol->string (Var-name L)) global-count))
					      ((= (car (m:entry-value e)) 1))
					      ((= (cdr (m:entry-value e)) 1)))
					   (make-App (make-Var 'map! a1) (list F L) a2)))
   (($ App ($ Var 'append a1) (NL . L*) a2) (and-let* (((not (l:member (Var-name NL) non-linear-type)))
					      (e (m:member ord (symbol->string (Var-name NL)) global-count))
					      ((= (car (m:entry-value e)) 1))
					      ((= (cdr (m:entry-value e)) 1)))
					   (make-App (make-Var 'map! a1) (list F L) a2)))
   ))


(define (attribute-union ord merge m1 m2)
  (define (f e m)
    (cond
     ((m:member ord (m:entry-key e) m) => 
      (lambda (c) (m:insert ord (m:new-entry (m:entry-key e) 
					     (merge (m:entry-value e) (m:entry-value c)))
			    m)))
     (else (m:insert ord e m))))
  (m:fold ord f m1 m2))


(define (count-def/use-join e1 e2)
  (cons (+ (car e1) (car e2))
	(+ (cdr e1) (cdr e2))))




(define count-def/use 
  (define join (e1 e2)
    (cons (+ (car e1) (car e2))
	  (+ (cdr e1) (cdr e2))))
  (define def/use
    (match-lambda 
     (($ Set ($ Var N a1) V2 a) (make-Set (make-Var N (set1 N

