
(define (inherited-attribute component attr)
  (lambda (tree)
    (or (component tree)
	(let* ((in (get-attribute this attr))
	       (put (cut put-attribute <> attr in)))
	  (match tree
		 (($ Define V B) 
		  (make-Define (put V) (put B) in))

		 (($ Lambda P* B) 
		  (make-Lambda (map (lambda (P) (put P)) P*)
			       (put B)
			       in))

		 (($ Set V V2) 
		  (make-Set (put V) (put V2) in))

		 (($ If C T E) 
		  (make-If (put C) (put T) (put E) in))

		 (($ Begin S E)
		  (make-Begin (put S) (put E) in))

		 (($ Let V E B) 
		  (make-Let (put V) (put E) (put B) in))

		 (($ Rec D+ B)
		  (let ((newDs
			 (map (match-lambda
			       ((V E) (list (put V) (put E))))
			      D+)))
		    (make-Rec newDs (put B) in)))

		 (($ App F A*)
		  (make-App (put F)
			    (map (lambda (A) (put A)) A*)
			    in)))))))

(define (synthesized-attribute component merge attr)
  (lambda (tree)
    (or (component tree)
	(let ((get (cut get-attribute <> attr))
	      (put (cut put-attribute tree attr <>)))
	  (match tree
		 (($ Define V B) 
		  (put (merge (get V) (get B))))

		 (($ Lambda P* B) 
		  (put (l:fold merge (get B)
			       (map (lambda (P) (get P)) P*))))

		 (($ Set V V2) 
		  (put (merge (get V) (get V2))))

		 (($ If C T E) 
		  (put (merge (get C) (get T) (get E))))

		 (($ Begin S E)
		  (put (merge (get S) (get E))))

		 (($ Let V E B) 
		  (put (merge (get V) (get E) (get B))))

		 (($ Rec D+ B)
		  (let ((val (map (match-lambda 
				   ((V E) (merge (get V) (get E))))
				  D+)))
		    (put (merge (get B) val))))

		 (($ App F A*)
		  (let ((val (l:fold merge (get F)
				     (map (lambda (A) (get A)) A*))))
		    (put val))))))))



;; bottom up
(define fv-merge set:union)

(define free-variable-analysis
  (lambda (tree)
    (let ((get (cut get-attribute <> 'free-variable))
	  (put (cut put-attribute tree 'free-variable <>)))
      (match tree 
	     (($ Let ($ Var V) E B)
	      (let ((free (fv-merge (set:difference (get B)
						    (set:new-set V))
				    (get E))))
		(put free)))

	     (($ Lambda Ps B)
	      (put (set:difference (get B) (map Var-name Ps))))

	     (($ Rec Ds B)
	      (map (match-lambda ((($ Var V) _) V)) Ds)
	      (l:fold-right fv-merge (get B) 
			    (map (match-lambda ((_ E) (get E))) Ds)))

	     (($ Var V) (put (set:new-set V)))

	     ((? Value?) (set:new-set))))))

;; top down 

(define count-closure-depth
  (match-lambda 
   (($ Lambda Ps B)
    (let ((count (+ 1 (get-attribute this 'depth))))
      (let ((term (make-Lambda Ps (put-attribute B 'depth count))))
	(put-attribute this 'depth count))))))

(define count-closure-depth-analysis 
  (strategy (try count-closure-depth) inherited-attribute))



;; repmin
(define find-min-rule
  (match-lambda
   (($ Value (? number? V)) (put-attribute this 'value V))
   ((or (? Var?) (? Value)) (put-attribute this 'value max-value))))

(define find-min-rule
  (strategy (bottomup (seq find-min (synthesized-attributes min 'value)))))


(define rep-min
  (($ Value (? number? V)) (make-Value (get-attribute this 'value))))

(define rep-min-rule
  (strategy (topdown (seq (inherited-attribute 'value) (try rep-min)))))


;; free variables

(define free-variables
  (match-lambda
   (($ Var V)
    (let ((bvars (get-attribute this 'bound-variables)))
      (put-attribute this 'free-variables 
		     (if (set:member V bvars)
			 (set:new-set V)
			 (set:new-set)))))

   ((? Value?) (put-attribute this 'free-variables (set:new-set)))))

(define free-variables-analysis
  (strategy
   (bottomup (choice free-variables
		     (synthesized-attribute set:union 'free-variables)))))

