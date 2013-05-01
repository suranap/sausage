
;; @ Low-level strategy operations for lists

;; These three definitions will allow the advanced strategies to work on lists. The lists are 


(define all				; strategy succeeds on all children
  (lambda (s)
    (match-lambda
     (('lambda ?attr ?sig ?b)
      (and-let* (b (s ?b)) `(lambda ,?attr ,?sig ,b)))

     (('case-lambda ?attr (?sigs ?bs) ...) 
      (and-let* (bs (and-map s ?bs)) `(case-lambda ,?attr ,@(map list ?sigs bs))))

     (('let ?attr (?v ?e) ?b) 
      (and-let* (v (s ?v)) (e (s ?e)) (b (s ?b)) `(let ,?attr (,v ,e) ,b)))

     (('set! ?attr ?v ?e)
      (and-let* (v (s ?v)) (e (s ?e)) `(set! ,?attr ,v ,e)))

     (('if ?attr ?p ?c ?a)
      (and-let* (p (s ?p)) (c (s ?c)) (a (s ?a)) `(if ,?attr ,p ,c ,a)))

     (('app ?attr ?es ...)
      (and-let* (es (and-map s ?es)) `(app ,?attr ,@es)))

     (?e ?e))))


(define some				; strategy succeeds on 1+ children
  (let ((or-f (lambda (x y) (or x y))))
    (lambda (s)
      (match-lambda 
       (('lambda ?attr ?sig ?b)
	(and-let* (b (s ?b)) `(lambda ,?attr ,?sig ,b)))

       (('case-lambda ?attr (?sigs ?bs) ...) 
	(let ((bs (map s ?bs)))
	  (and (foldl or-f #f bs) `(case-lambda ,?attr ,@(map list ?sigs (map or-f bs ?bs))))))

       (('let ?attr (?v ?e) ?b) 
	(let ((v (s ?v)) (e (s ?e)) (b (s ?b))) 
	  (and (or v e b)		; ensure at least one succeeded
	       `(let ,?attr (,(or v ?v) ,(or e ?e)) ,(or b ?b)))))

       (('set! ?attr ?v ?e)
	(let ((v (s ?v)) (e (s ?e))) 
	  (and (or v e) `(set! ,?attr ,(or v ?v) ,(or e ?e)))))

       (('if ?attr ?p ?c ?a) 
	(let ((p (s ?p)) (c (s ?c)) (a (s ?a)))
	  (and (or p c a) `(if ,?attr ,(or p ?p) ,(or c ?c) ,(or a ?a)))))

       (('app ?attr ?es ...)
	(let ((es (map s ?es)))
	  (and (foldl or-f #f es) `(app ,?attr ,@(map or-f es ?es)))))

       (?e #f)))))


(define one				; strategy succeeds after only 1 child succeeds
  (lambda (s)
    (match-lambda 
     (('lambda ?attr ?sig ?b)
      (and-let* (b (s ?b)) `(lambda ,?attr ,?sig ,?b)))

     (('case-lambda ?attr (?sigs ?bs) ...) 
      (and-let* (bs (or-map s ?bs))
		`(case-lambda ,?attr ,@(map list ?sigs bs))))

     (('let ?attr (?v ?e) ?b) 
      (or (and-let* (v (s ?v)) `(let ,?attr (,v ,?e) ,?b))
	  (and-let* (e (s ?e)) `(let ,?attr (,?v ,e) ,?b))
	  (and-let* (b (s ?b)) `(let ,?attr (,?v ,?e) ,b))))

     (('set! ?attr ?v ?e)
      (or (and-let* (v (s ?v)) `(set! ,?attr ,v ,?e))
	  (and-let* (e (s ?e)) `(set! ,?attr ,?v ,e))))

     (('if ?attr ?p ?c ?a) 
      (or (and-let* (p (s ?p)) `(if ,?attr ,p ,?c ,?a))
	  (and-let* (c (s ?c)) `(if ,?attr ,?p ,c ,?a))
	  (and-let* (a (s ?a)) `(if ,?attr ,?p ,?c ,a))))

     (('app ?attr ?es ...)
      (and-let* (es (or-map s ?es)) `(app ,?attr ,@es)))

     (?e #f))))



