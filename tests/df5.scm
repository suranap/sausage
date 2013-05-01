
;; Actually, if everything is in normalized form, then I can just
;; match on larger patterns and patch things up that way.

;; DAMN! Just put a pointer to the parent term and be done with it.

(define (inherited-path3 term)
  (match term
    (($ Define V B a)  (set! (scheme-attr B) term) #f)

    ;; the use of V2 occurs before the set. 
    (($ Set V V2 a)    (set! (scheme-attr V) term) (set! (scheme-attr V2) a) #f)

    (($ Lambda P* B a) (set! (scheme-attr B) term) #f)


    ;; The path from S is easy. The path for E is every possible exit
    ;; from S. For example, in (begin (if a b c) E), the path from
    ;; program entry to E goes through a and (b or c).
;    (($ Begin S E a)   
;     (set! (scheme-attr S) a)
;     (set! (scheme-attr E) (list (delay (map (lambda (exit) (cons exit (scheme-attr exit)))
;					     (collect-exit-terms S)))))
;     #f)

    ;; The path for E is easy. The path from B to program entry must
    ;; go through Let to initialize V. When someone traverses the
    ;; list, they need to collect the exit nodes in E and follow those
    ;; paths to entry. 
    (($ Let V E B a)
     (set! (scheme-attr E) a)
     (set! (scheme-attr B) term)
     #f)

    (($ If C T E a)    
     (set! (scheme-attr C) a)
     (set! (scheme-attr T) C)
     (set! (scheme-attr E) C)
     #f)

    ))
