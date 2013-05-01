
(define bound-variable-analysis-helper
  (match-lambda
   (($ Lambda Ps B)
    (let ((bounded (get-attribute this 'bound-variables))
	  (params (map Var-name Ps)))
      (let ((newB (put-attribute B 'bound-variables (set:union params bounded))))
	(make-Lambda Ps newB (attributes this)))))

   (($ Rec Fs B)
    (let* ((variables (map (match-lambda ((($ Var V) _) V)) Fs))
	   (bounded (get-attribute this 'bound-variables))
	   (newBounded (set:union variables bounded)))
      (let ((newFs (map (match-lambda 
			 ((V E) (list V (put-attribute E 'bound-variables newBounded))))
			Fs)))
	(make-Rec newFs (put-attribute B 'bound-variables newBounded)))))

   (($ Let (and ($ Var N) V) E B)
    (let ((bounded (get-attribute this 'bound-variables)))
      (let ((newB (put-attribute B 'bound-variables (set:union (set:new-set N) bounded))))
	(make-Lambda Ps newB (attributes this)))))))

(define bound-variable-analysis (top-down-analysis bound-variable-analysis-helper))


(define match-variable-rule
  (match-lambda
   (('match-lambda clauses ...)
    (let ((bounded (get-attribute this 'bound-variables)))
      `(match-lambda-aux ,bounded ,@clauses)))))

