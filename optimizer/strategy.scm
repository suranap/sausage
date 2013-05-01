

;; @ Higher-order rewrite combinators

;; A rewrite strategy describes how to navigate a tree and apply
;; transformations. Higher-order strategies can also be described,
;; such that strategies can be combined at runtime. This library must
;; be provided implementations for the special low-level operations
;; one, all and some.


;; @ Reference

;;  @inproceedings{ vbt98,
;;    author = {Visser, Eelco and Benaissa, Zine-el-Abidine and Tolmach, Andrew},
;;    title = {Building Program Optimizers with Rewriting Strategies},
;;    booktitle = {Proceedings of the third ACM SIGPLAN International Conference on Functional Programming (ICFP'98)},
;;    year = {1998},
;;    month = {September},
;;    pages = {13--26},
;;    publisher = {ACM Press},
;;    url = {citeseer.ist.psu.edu/visser98building.html} }


;; @ High-level strategy syntax

;; (strategy sig rule) - top-level
;;   sig - combine higher-order strategies

;; rule commands: 
;;   (seq rule ...) - really 'and' of rules 
;;   (choice rule ...) - really 'or' of rules
;;   (rec name rule) - introduce recursive binding into rule
;;   (one rule) - apply rule to children, #t if 1 child succeeds
;;   (all rule) - apply rule to children, #t if all children succeed
;;   (some rule) - apply rule to children, #t if some children succeed
;;   atom - a base rule to apply 

;; This is slightly complicated by higher-order strategies.

(define-syntax strategy
  (syntax-rules ()
    ((strategy ?rule)
     (lambda (var)
       (strategy-aux ?rule var)))
    ((strategy ?curry-sig ?rule)
     (lambda ?curry-sig
       (lambda (var)
	 (strategy-aux ?rule var))))))

(define-syntax strategy-aux
  (syntax-rules (seq choice rec all one some)
    ((strategy-aux (seq ?rule) ?var)
     (strategy-aux ?rule ?var))
    ((strategy-aux (seq ?rule ?rules ...) ?var)
     (let ((t (strategy-aux ?rule ?var)))
       (and t (strategy-aux (seq ?rules ...) t))))

    ((strategy-aux (choice ?rule ...) ?var)
     (or (strategy-aux ?rule ?var) ...))
    
    ((strategy-aux (rec ?name ?rule) ?var)
     (letrec ((?name (lambda (var) (strategy-aux ?rule var)))) 
       (strategy-aux ?name ?var)))

    ((strategy-aux (?ho-strategy ?expr ...) ?var)
     ((?ho-strategy (lambda (var) (strategy-aux ?expr var)) ...) ?var))

    ((strategy-aux ?atom ?var)
     (?atom ?var))))


;; @ Traversal Strategies

;; These are copied from Appendix B of this same paper

;; @@ Primitives

(define fail (lambda (term) #f))		; unconditional failure
(define id (lambda (term) term))		; unconditional success


;; @@ Strategies


;; simple strategies
(define try (strategy (s) (choice s id))) ; attempt s 
(define repeat (strategy (s) (rec x (try (seq s x))))) ; apply s*
(define repeat1 (strategy (s) (seq s (repeat s)))) ; apply s+

;; The rule must succeed for the root and all children
(define bottomup (strategy (s) (rec x (seq (all x) s))))
(define topdown (strategy (s) (rec x (seq s (all x)))))

;; combine bottomup and topdown into one rule
(define downup (strategy (d u) (rec x (seq d (all x) u))))

;; For the following six rules, remember that 'choice' will
;; short-circuit when the first term succeeds. Therefore, these rules
;; will not apply the rule to all possible terms. For example, in
;; somebu if 'some x' succeeds, it will not apply the rule to the root
;; term.

;; The rule must succeed for the root or only one child
(define oncebu (strategy (s) (rec x (choice (one x) s))))
(define oncetd (strategy (s) (rec x (choice s (one x)))))

;; The rule must succeed for the root or some children. 

(define somebu (strategy (s) (rec x (choice (some x) s))))
(define sometd (strategy (s) (rec x (choice s (some x)))))

;; The rule must succeed for the root or all children
(define allbu (strategy (s) (rec x (seq (all x) (try s)))))
(define alltd (strategy (s) (rec x (choice s (all x)))))

;; These behave like somebu and sometd, where 'choice' does not
;; short-circuit when the first term succeeds; instead, it will also
;; try to apply the second part of the rule.
(define manybu (strategy (s) (rec x (choice (seq (some x) (try s)) s))))
(define manytd (strategy (s) (rec x (choice (seq s (all (try x))) (some x)))))

;; The rule is applied to the root, then to some children, and finally
;; to the root again.
(define manydownup (strategy (s) (rec x (choice (seq s (all (try x)) (try s)) 
						(seq (some x) (try s))))))


;; Apply a rule until fixpoint (no more changes). When order matters,
;; outermost moves topdown and innermost move bottomup. Otherwise,
;; reduce applies the rule everywhere more efficiently(?).
(define outermost (strategy (s) (repeat (oncetd s))))
(define innermost (strategy (s) (repeat (oncebu s))))
(define reduce (strategy (s) (repeat (manybu s))))

