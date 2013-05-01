


;; -----------------------------------

(define *case-lambda-alst* '((map . ((2 . map1) (3 . map2) (4 . map3)))))
(define (case-lambda-link? name)
  (assv name *case-lambda-alst*))  

(define case-lambda-linker-rule
  (match-lambda
   (($ App ($ Var (? case-lambda-link? F)) A*)
    (and-let* ((bindings (case-lambda-link? F))
	       (fbind (assv (length A*) (cdr bindings))))
	      (make-App (make-Var (cdr fbind) '()) A* '())))))

;; --------------------------------------

(define 


;; ---------------------------------------

(define expand-nary-procedures-rule
  (match-lambda
   ;; append
   (($ Let V ($ App (and ($ Var 'append) F) (A1 A2 A3 . A*)) B)
    (let ((T (gensym-Var)))
      (make-Let T (make-App F (cons A2 (cons A3 A*)) '())
		(make-Let V (make-App F (list A1 T) '()) B '()) '())))

   ;; cons* 
   (($ App ($ Var 'cons*) (A1)) A1)
   (($ App ($ Var 'cons*) (A1 A2)) (make-App (make-Var 'cons '()) (list A1 A2) '()))
   (($ Let V ($ App (and ($ Var 'cons*) F) (A1 A2 A3 . A*)) B)
    (let ((T (gensym-Var)))
      (make-Let T (make-App F (l:cons* A2 A3 A*) '())
		(make-Let V (make-App (make-Var 'cons '()) (list A1 T) '()) B '()) '())))

   ;; list=
   (($ Let V ($ App (and ($ Var 'list=) F) (P A1 A2 A3 . A*)) B)
    (let ((T (gensym-Var)))
      (make-Let T (make-App F (l:cons* P A2 A3 A*)) '())
		(make-Let V (make-App F (list P A1 T) '()) B '()) '()))))


;; --------------------------------------
(define inline-map-rule
  (match-lambda 
   ;; map f l1
   (($ Let V ($ App ($ Var 'map) (F L1)) B)
    (let* ((loop (gensym-Var))
	   (maprec (make-map1-loop (Var-name F) (Var-name loop))))
      (set! (Rec-body maprec) 
	    (make-Let V (make-App loop (list L1) '()) B '()))
      maprec))

   ;; map f l1 l2
   (($ Let V ($ App ($ Var 'map) (F L1 L2)) B)
    (let* ((loop (gensym-Var))
	   (maprec (make-map2-loop (Var-name F) (Var-name loop))))
      (set! (Rec-body maprec) 
	    (make-Let V (make-App loop (list L1 L2) '()) B '()))
      maprec))))


(define (make-map1-loop f loop)
  (parse `(letrec ((,loop (lambda (lst)
			    (let ((v_21 lst))
			      (let ((v_22 (null? v_21)))
				(if v_22
				    lst
				    (let ((v_23 lst))
				      (let ((v_24 (car v_23)))
					(let ((v_25 (,f v_24)))
					  (let ((v_26 lst))
					    (let ((v_27 (cdr v_26)))
					      (let ((v_28 (,loop v_27)))
						(cons v_25 v_28)))))))))))))
	    body)))

(define (make-map2-loop f loop)
  (parse `(letrec ((,loop (lambda (l1 l2)
			    (let ((v_30 l1))
			      (let ((v_31 (null? v_30)))
				(let ((v_32 l2))
				  (let ((v_33 (null? v_32)))
				    (let ((v_34 (or v_31 v_33)))
				      (if v_34
					  '()
					  (let ((v_35 l1))
					    (let ((v_36 (car v_35)))
					      (let ((v_37 l2))
						(let ((v_38 (car v_37)))
						  (let ((v_39 (,f v_36 v_38)))
						    (let ((v_40 l1))
						      (let ((v_41 (cdr v_40)))
							(let ((v_42 l2))
							  (let ((v_43 (cdr v_42)))
							    (let ((v_44 (,loop v_41 v_43)))
							      (cons v_39
								    v_44))))))))))))))))))))
	    body)))

