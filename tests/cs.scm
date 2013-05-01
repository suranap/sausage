
;; Test out the constraint system for this code:

(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

;; The AST looks like this:

(define ast1 '(define fact
		(lambda (n|1)
		  (let (v_1 n|1)
		    (let (v_2 (< v_1 2))
		      (if v_2
			  1
			  (let (v_3 n|1)
			    (let (v_4 n|1)
			      (let (v_5 (- v_4 1))
				(let (v_6 (fact v_5))
				  (* v_3 v_6)))))))))))

;; Build a simple sample constraint graph
(let (v 1) v)

(define make-constraint-network
  (match-lambda      
   (($ Define V B _) 
   (($ Set V V2 _)   
   (($ Lambda P+ B _)
   (($ Begin S E _)  
   (($ Let V E B _)  
   (($ If C T E _)   
   (($ App F A* _)   
   (($ Var N _)      
   (($ Value D _)    