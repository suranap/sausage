
;; @ Rewrite Strategies

(define-interface strategy-core
  (export one some all))

(define-module (make-strategy core)
  (structure 
   (export (strategy :syntax)
	   all some one			; re-export core strategies
	   fail id
	   try
	   repeat repeat1
	   bottomup topdown
	   downup
	   oncebu oncetd
	   somebu sometd
	   manybu manytd
	   manydownup
	   alltd allbu
	   reduce
	   outermost innermost)
   (open scheme core srfi-1 srfi-8)
   (files strategy)))

(define-structure core-tree-strategy strategy-core
  (open scheme srfi-1 srfi-2)
  (files tree-strategy))

(def strategy (make-strategy core-tree-strategy))


