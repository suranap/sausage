
;; bottomup
(define find-last-expression
  (lambda (tree)
    (let ((get (cut get-attribute tree 'cfg))
	  (put (cut put-attribute tree 'cfg <>)))
      (match tree
	     (($ Define V B) 
	      (put (get B)))

	     (($ If C T E) 
	      (put (list (get T) (get E))))

	     (($ Begin S E)
	      (put (get E)))

	     (($ Let V E B) 
	      (put (get B)))

	     (($ Rec D+ B)
	      (put (get B)))

	     ((? Lambda?) (put tree))
	     ((? Set?) (put tree))
	     ((? App?) (put tree))
	     ((? Var?) (put  tree))
	     ((? Value?) (put tree))
	     ))))



;; bottomup
(define thread-ast
  (lambda (tree)
    (let ((get (cut get-attribute tree 'cfg <>))
	  (put (cut put-attribute tree 'cfg <>)))
      (match tree
	     (($ Define V B) 
	      (put (get B)))

	     (($ If C T E) 
	      )

	     (($ Begin S E)
	      (

	     (($ Let V E B) 
	      (put B tree)
	      

	     (($ Rec D+ B)
	      (put (get B)))

	     ((? Lambda?) (put tree))
	     ((? Set?) (put tree))
	     ((? App?) (put tree))
	     ((? Var?) (put  tree))
	     ((? Value?) (put tree))
	     ))))