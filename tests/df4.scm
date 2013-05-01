
;; actually, can't use #t,#f as return values, because #f means the
;; rule failed. Instead, use a cell.

(define (some x) (cons 'some x))
(define none (cons 'none '()))
(define (some? x) (eq? (car x) 'some))
(define (none? x) (eq? x none))
(define some-val cdr)

(define (scheme-synthesize join default tree)
  (match tree
   (($ Define V B)  (join V B))
   (($ Set V V2)    V2)
   (($ Lambda P* B) B)			; do something about P*
   (($ Begin S E)   (join S E))
   (($ Let V E B)   (join V (join E B)))
   (($ If C T E)    (join C (join T E)))
   (($ App F A*)    (l:fold join F A*))
   ((? Var? e) default)
   ((? Value? e) default)))

(define (some-or x y)
  (some (or (some-val x) (some-val y))))



;; exit when it finds any assignment to var
(define (variable-assignment exit var)
  (match-lambda 
   (($ Set ($ Var V) _) (=> fail) 
    (if (equal? V var) (exit #t) (fail)))))



(define (replace-variable var replacement)
  (match-lambda
   (($ Var V) (=> fail)
    (if (equal? V var) 
	replacement
	(fail)))))

(define constant-propagation
  (match-lambda 
   (($ Let ($ Var V) (and ($ Value D) C) B) (=> fail)
    (let ((found (call-with-current-continuation
		  (lambda (exit)
		    ((oncetd (variable-assignment exit V)) B)))))
      (if found (fail)
	  (or ((manybu (replace-variable V C)) B)
	      (fail)))))))


(define (variable-use-count var)
  (match-lambda
   (($ Var V) (=> fail)
    (if (equal? V var) 1 (fail)))
   (term (scheme-synthesize + 0 term))))

(define (remove-assignments var)
  (match-lambda 
   (($ Set ($ Var V) Rhs) (=> fail)
    (if (equal? V var) 
	(make-Value 'unspecified '())
	(fail)))))


(define dead-variable-elimination
  (match-lambda
   (($ Let ($ Var V) E B) (=> fail)
    (let ((count ((bottomup (variable-use-count V)) B)))
      (if (= count 0)
	  (make-Begin E ((manybu (try (remove-assignments V))) B))
	  (fail))))))

(define redundant-variable-elimination
  (match-lambda 
   (($ Let ($ Var V) (and ($ Var V2) C) B) (=> fail)
    (let ((found (call-with-current-continuation
		  (lambda (exit)
		    ((oncetd (variable-assignment exit V)) B)))))
      (if found (fail)
	  (or ((manybu (replace-variable V C)) B)
	      (fail)))))))
	  



;; At each node, generate a list of statements from the node to the
;; program entry point representing that particular program path, or
;; control flow. The list will actually be backwards, where the
;; program entry point will be the last element in the list.

(define (inherited-path term)
  (match term
    (($ Define V B a)  (make-Define V (copy-node B (cons term a)) a))
    (($ Set V V2 a)    (make-Set V (copy-node V2 (cons term a)) a))
    (($ Lambda P* B a) (make-Lambda P* (copy-node B (cons term a)) a))
    (($ Begin S E a)   (make-Begin (copy-node S (cons term a)) 
				   (copy-node E (cons S (cons term a)))
				   a))
    (($ Let V E B a)   (make-Let V (copy-node E (cons term a)) 
				 (copy-node B (cons E (cons term a))) a))
    (($ If C T E a)    (make-If (copy-node C (cons term a))
				(copy-node T (cons C (cons term a)))
				(copy-node E (cons C (cons term a)))
				a))
    (($ App F A* a)    (let ((path (cons term a)))
			 (make-App (copy-node F path)
				   (map (lambda (t) (copy-node t path)) A*)
				   a)))
    (($ Var N a)       term)
    (($ Value D a)     term)))


;; The path to the program entry is stored in the attribute slot. This
;; path is a list of terms from *before* this node is executed to the
;; program entry. That is, the very first statement executed should be
;; that last term in the list. When the control flow branches it is
;; represented as a list of possible paths. 

;; This rule runs topdown. Since the paths of the subtrees hasn't been
;; computed yet, the path to entry has to be computed on demand for
;; certain kinds of nodes.

(define inherited-path2
  (match-lambda
   (($ Define V B a)  (let ((term (make-Define V #f a))) 
			(set! (Define-body term) (copy-node B (cons term a)))
			term))

    ;; the use of V2 occurs before the set. 
   (($ Set V V2 a)    (make-Set V (copy-node V2 a) a))

   (($ Lambda P* B a) (let ((term (make-Lambda P* #f a)))
			(set! (Lambda-body term) (copy-node B (cons term a)))
			term))

    ;; The path from S is easy. The path for E is every possible exit
    ;; from S. For example, in (begin (if a b c) E), the path from
    ;; program entry to E goes through a and (b or c).
    (($ Begin S E a)   (let ((term (make-Begin (copy-node S a) #f a)))
			 ;; when you get there, follow the exit terms in S
			 (set! (Begin-expr term) (copy-node E (list term)))
			 term))

    ;; The path for E is easy. The path from B to program entry must
    ;; go through Let to initialize V. When someone traverses the
    ;; list, they need to collect the exit nodes in E and follow those
    ;; paths to entry. 
    (($ Let V E B a)   (let ((term (make-Let V (copy-node E a) #f a)))
			 ;; when you get here, follow the exit terms in E
			 (set! (Let-body term) (copy-node B (list term)))
			 term))

    (($ If C T E a)    (let ((p (cons C a)))
			 (make-If (copy-node C a) (copy-node T p) (copy-node E p) a)))

    (($ App F A* a)    (make-App (copy-node F a) 
				 (map (lambda (t) (copy-node t a)) A*)
				 a))

    ((and (? Var?) term)       term)
    ((and (? Value?) term)     term)))

;; locate the exit terms in the tree
(define collect-exit-terms
  (match-lambda
   (($ Define V B) (collect-exit-terms B))
   (($ Lambda P* B) (collect-exit-terms B))
   (($ Begin S E) (collect-exit-terms E))
   (($ Let V E B) (collect-exit-terms B))
   ;; merge points are represented by lists of paths 
   (($ If C T E) (append (collect-exit-terms T) (collect-exit-terms E)))
   (e (list e))))


(define (copy-node node attr)
  (match node
    (($ Define V B)  (make-Define V B attr))
    (($ Set V V2)    (make-Set V V2 attr))
    (($ Lambda P* B) (make-Lambda P* B attr))
    (($ Begin S E)   (make-Begin S E attr))
    (($ Let V E B)   (make-Let V E B attr))
    (($ If C T E)    (make-If C T E attr))
    (($ App F A*)    (make-App F A* attr))
    (($ Var N)       (make-Var N attr))
    (($ Value D)     (make-Value D attr))))

(define display-path
  (match-lambda
    (($ Define V B)  (print "define"))
    (($ Set V V2)    (print "set"))
    (($ Lambda P* B) (print "lambda"))
    (($ Begin S E)   (print "begin"))
    (($ Let V E B)   (print "let: " (Var-name V))
     (let ((exits (collect-exit-terms E)))
       (for-each (lambda (term) 
		   (display-path term)
		   (if (null? (scheme-attr term)) (error "No scheme attrs."))
		   (for-each display-path (scheme-attr term)))
	    exits)))
    (($ If C T E)    (print "if"))
    (($ App F A*)    (print "app"))
    (($ Var N)       (print "var: " N))
    (($ Value D)     (print "value"))))

(define (linearize-path path)
  (define expand-node
    (match-lambda
     ((and ($ Let _ E _) term)
      (let ((exits (collect-exit-terms E)))
	(map linearize-path exits)))))

  (if (null? path) path
      (cons (or (expand-node (car path)) (car path)) 
	    (linearize-path (cdr path)))))


;; For every pure function, reuse whenever possible
(define common-subexpression-elimination 
  (match-lambda 
   (($ Let V ($ App F A*) B route) (=> fail)	; also check that E is pure
    (print "MATCH Let for Var: " V "; local variables: " (local-variables route))
    (or (match-let ((($ Let V2 E2 _)
		     (l:find (match-lambda ; also check that V2 is not assigned
			      (($ Let V2 ($ App F A*) _) #t))
			     route)))
		   (make-Let V V2 B route))
	(fail)))))


(define (local-variables route)
  (map (match-lambda (($ Let V _ _) V))
       (l:filter (match-lambda (($ Let V _ _) #t)) route)))


;; Find the return exprs for a lambda
(define lambda-type
  (match-lambda
   (($ Lambda P* B)
    (display-return-exprs B))))

(define (find-return-exprs term)
  (match term
    (($ Define V B)  B)
    (($ Set V V2)    (some (list (make-Value 'undefined '()))))
    (($ Lambda P* B) B)
    (($ Begin S E)   E)
    (($ Let V E B)   B)
    (($ If C T E)    (some (append (some-val T) (some-val E))))
    ;; All the App parameters are wrapped by some. Unwrap them.
    (($ App F A*)    (some (make-App (car (some-val F)) (map some-val A*) '())))
    (($ Var N)       (some (list term)))
    (($ Value D)     (some (list term)))))
   
(define (display-return-exprs term)
  (match term
    (($ App F A*)    (some (make-App (car (some-val F)) (map some-val A*) '())))
    (($ Var N)       (some (list term)))
    (($ Value D)     (some (list term)))))

(define memv-optimization
  (match-lambda 
   (($ Let V ($ App ($ Var 'memv) K ($ Value L)) B a) (=> fail)
    ;; fold over L to generate (Let Gensym (eqv? K (e in L))
;    (if (> (length L) 3) (fail)
;	(lambda (val) (make-App (make-Var 'eqv? '()) K (make-Value val '())))
    )
   (($ App ($ Var 'memv) K ($ Value (e)))
    (make-App (make-Var 'eqv? '()) K (make-Value e '()) '()))))




;; ----------------------------------------------------
;; Attributes

(define-attribute pure (function-name))
==>
(define pure
  (case-lambda
   ((tree function-name)
    ;; find the attribute 'pure dangling off this tree, then search
    ;; for function-name. Return the associated value.
    )
   ((tree function-name result)
    ;; store ['pure -> ((function-name . result) ...)] in tree
    )))


(define-attribute variable-usage-count (variable)
  (bottomup (variable-use-count variable)))
==>
(define variable-usage-count
  (case-lambda
   ((tree variable)
    ;; see if the result is already cached in tree, otherwise ...
    (let ((result ((bottomup (variable-use-count variable)) tree)))
      (variable-usage-count tree variable result)
      result))
   ((tree variable result)
    ;; store result in tree
    )))


(define (set-Scheme-attr! node val)
  (cond
   ((Define? node) (set-Define-attr! node val))
   ((Set? node) (set-Set-attr! node val))
   ((Lambda? node) (set-Lambda-attr! node val))
   ((Begin? node) (set-Begin-attr! node val))
   ((Let? node) (set-Let-attr! node val))
   ((If? node) (set-If-attr! node val))
   ((App? node) (set-App-attr! node val))
   ((Var? node) (set-Var-attr! node val))
   ((Value? node) (set-Value-attr! node val))))