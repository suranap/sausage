
;; @ Sausage Scheme compiler

(define-structure test (export)
  (open (modify scheme (hide set!)) srfi-17
	pp match macros normalize parse 
	scheme-strategy ast 
	(with-prefix map m:) (with-prefix bag b:) (with-prefix srfi-1 l:))
  (begin))

