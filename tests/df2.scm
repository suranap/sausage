
;; Dataflow Library (second attempt)

;; @ Introdution

;; Attributes should be stored in functional data structures. 


;; Attributes are either inherited (top-down) or synthesized (bottom-up). 

(define attr cons)
(define inherit car)
(define synth cdr)

;; Attributes are stored in a map, but the value is user-defined. 
(define empty-attr 
  (let ((m (m:new (m:new-order string=? string<?)))) (attr m m)))




