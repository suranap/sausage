
;; Attribute "Grammars"

;; Many simple analyses can be computed by propagating attributes up
;; and down a tree. These tree walkers handle the default case of
;; moving attributes up and down the tree. These should be combined
;; with a specific attribute handler to compute attribute values.

;; Someday, memoize this so it's more efficient.

;; For now, attributes will be an assoc list of ((join function . attribute) ...)

;; @ Inherited Attributes

; (define (inherited-attributes join)


;; @ Synthesized Attributes

;; Move attributes BOTTOMUP in a tree.
(define (synthesized-attributes join)
  (match-lambda 
   (($ Define V B a)  (make-Define V B (join (Scheme-attr V) (Scheme-attr B))))
   (($ Set V V2 a)    (make-Set V V2 (join (Scheme-attr V) (Scheme-attr V2))))
   (($ Lambda P+ B a) (make-Lambda P+ B (fold join (Scheme-attr B) (map Scheme-attr P+))))
   (($ Begin S* a)    (make-Begin S* (reduce (map Scheme-attr S*))))
   (($ Let V E B a)   (make-Let V E B (join (Scheme-attr V) (join (Scheme-attr E) (Scheme-attr B)))))
   (($ If C T E a)    (make-IF C T E (join (Scheme-attr C) (join (Scheme-attr T) (Scheme-attr E)))))
   (($ App F A* a)    (make-App F A* (fold join (Scheme-attr F) (map Scheme-attr A*))))
   ((? Var? e) e)
   ((? Value? e) e)))

;; TODO: Should Var and Value always return an expression? When should
;; they fail? It needs to do something reasonable for ALL, SOME and
;; ONE.


;; @ Attribute management

