
;; improve functional maps

(define (mark-variable name f)
  (match-lambda 
   (($ Var N a) (=> fail)
    (if (equal? N name)
	(make-Var N (f a))
	(fail)))))

(define (mark-linear-map-type attribute)
  (attr (inherit attribute)
	(m:insert "linear-map" #t (synth attribute))))

(define functional-map-type-rule
  (match-lambda 
   (($ Let V (and E ($ App ($ Var (or 'insert 'remove)) A*)) B a) (=> fail)
    (print "Trying: " V)
    (let ((s (synth a))
	  (Vstr (symbol->string (Var-name V))))
      (if (or (and-let* ((sets (m:member "set" s))) (b:member Vstr sets))	; no set!
	      (and-let* ((uses (m:member "use" s)) (c (b:member Vstr uses))) (not (= c 1)))) ; V not used only 1 time
	  (fail)
	  (let ((new-B ((oncebu (debug-strategy (mark-variable (Var-name V) mark-linear-map-type))) B)))
	    (if new-B			; this MUST be true
		(make-Let V E new-B a)
		(fail))))))))


(define linear-type?
  (match-lambda
   (($ Var N a) (m:member "linear-map" (synth a)))))

(define functional-map-rule
  (match-lambda 
   (($ App ($ Var 'insert a1) (and (Key Val (? linear-type? M)) A*) a2)
    (make-App (make-Var 'insert! a1) A* a2))
   (($ App ($ Var 'remove a1) (and (Key (? linear-type? M)) A*) a2)
    (make-App (make-Var 'remove! a1) A* a2))))



;; try to improve map and fold code

;; (map f l . ls) results in a new spine 
;; (map f l), where l is a linear spine, is (map! f l)
(define list-map-rule
  (match-lambda 
   (($ Let V ($ App ($ Var 'map ma) F (? linear-spine? L)) B a)
    (make-Let V (make-App (make-Var 'map! ma) F L) B a))
   (($ Let V (and A ($ App (and M ($ Var 'map)) F . A*)) B a)
    ;; tag V in B as a linear-spine type
    )))


(define (reverse l) (fold cons '() l))

(define (any pred l)
  (fold (lambda (e flag) (or (pred e) flag)) #f l))

(define (fold f i l)
  (let rec ((i i) (l l))
    (if (null? l)
	i
	(rec (f (car l) i) (cdr l)))))

;; try to inline fold into any
(let rec ((i #f) (l l))
  (if (null? l)
      i
      (let ((e (car l)) (flag i))
	(rec (f e flag) (cdr l)))))



(define-syntax fold 
  (syntax-rules ()
    ((fold f i l)
     (let rec ((in i) (lst l))
       (if (null? l)
	   i
	   (let ((e (car lst)) (flag in))
	     (rec (f e in) (cdr l))))))))


;; any after normalization
(letrec (rec|4 (lambda (i|5 l|5)
		 (let (v_19 (null? i|5))
		   (if v_19
		       i|5
		       (let (e|8 (car l|5))
			 (let (v_23 (f e|8 i|5))
			   (let (v_25 (cdr l|5))
			     (rec|4 v_23 v_25))))))))
  (rec|4 #f l))


(define (filter pred lis)
  (fold-right (lambda (e i) (if (pred e) (cons e lis) lis)) '() lis))


(define (append-map f l) (reduce-right append '() (map f l)))