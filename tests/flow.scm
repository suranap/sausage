
;; Flow analysis

;; topdown

(define (control-flow term)
  (match term

   (($ Define V B) 
    (set! (scheme-attr B) term) 
    #f)

   (($ Lambda P* B) 
    (set! (scheme-attr B) term) 
    #f)

   (($ Set V V2) 
    (set! (scheme-attr V2) (scheme-attr term)) 
    (set! (scheme-attr V) term)
    #f)

   (($ If C T E) 
    (set! (scheme-attr C) term)
    (set! (scheme-attr T) C)
    (set! (scheme-attr E) C) 
    #f)

   (($ Begin S E)
    (set! (scheme-attr S) term)
    ;; connect E to the last terms in S
    )

   (($ Let V ($ If P C A) B)
    #f)

   (($ Let V E B) 
    (set! (scheme-attr E) (scheme-attr term))
    (set! (scheme-attr B) term)
    #f)

   ;; still need (let v (app ...) b)

   (($ Rec D+ B)
    )

   (($ App F A*)
    (fold (lambda (a h) (set! (scheme-attr a) h) a) F A*)
    )

;;   (($ Var N) #f)
;;   (($ Value D) #f)
   ))