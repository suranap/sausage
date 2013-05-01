
;; @ Core Scheme Simplifications

;; These are just simple rewrite rules to simplify the AST.

(define nil '())
(define unspecified (make-Value 'unspecified nil))

;; One serious problem with this matcher is we don't have pattern guards.

;; many bottom-up
(define simple-syntactic-simplifications
  (match-lambda 

   ;; Set LeftHandSide RightHandSide 
   (($ Set (and L ($ Var N _)) (and R ($ Var M _)) a) 
    (and (eqv? N M) unspecified))	; make sure this is the last Set pattern

   ;; Begin Statement Expression
   (($ Begin nil nil a) unspecified)
   (($ Begin nil E a) E)
   (($ Begin ($ Value 'unspecified _) E a) E)
   (($ Begin S nil a) (make-Begin S unspecified a))

   ;; Let Variable Expression Body
   (($ Let _ E nil _) E)
   (($ Let _ E ($ Value 'unspecified _) _) E)
   (($ Let V E (? Set? S) B a) (make-Begin S (make-Let V unspecified B a) nil))
   (($ Let V ($ Begin S E _) B a) (make-Begin S (make-Let V E B a) nil))

   ;; If Condition Then Else
   (($ If C T nil a) (make-If C T unspecified a))
   (($ If C nil E a) (make-If C unspecified E a))
   (($ If ($ Value #f _) _ E _) E)
   (($ If ($ Value (not #f) _) T _ _) T)

   ))


