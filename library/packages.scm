
;; @ Andrew K Wright's pattern matcher and structure macros

(define-structure match-implementation
  (export
   match:expanders
   match:andmap
   gentemp
   gentemp-cell
   match:error
   match:set-error-control
   match:set-error
   match:syntax-err
   match:disjoint-predicates)
  (open scheme signals pp)
  (files match-implementation))

(define-interface match-interface
  (export
    (match :syntax)
    (match-lambda :syntax)
    (match-lambda* :syntax)
    (match-let :syntax)
    (match-let* :syntax)
    (match-letrec :syntax)
    (match-define :syntax)
    (defstruct :syntax)
    (define-structure :syntax)
    (define-const-structure :syntax)
    match:error
    match:set-error
    match:set-error-control

    define-record-discloser))

(define-structure match
  match-interface
  (open scheme record-types match-implementation)
  (for-syntax (open scheme record match-implementation))
  (files match)
  (begin
    (match:set-error (lambda (v) #f))
    (match:set-error-control 'error)))


;; @ Collections 

;; @@ Collection interfaces 

;; type 'a = = 'a * 'a
;; type 'a < = 'a * 'a
;; type ('a) order = 'a = * 'a <

(define-interface collection^
  (export new-order			; 'a = * 'a < -> ('a) order
	  new				; ('a) order -> (_) coll
	  size				; (_) coll -> int
	  member			; 'a * 'a coll -> 'a
	  insert			; 'a * 'a coll -> 'a coll
	  remove			; 'a * 'a coll -> 'a coll
	  fold				; ('a * 'i -> 'i) * 'i * 'a coll -> 'i
	  elements))			; 'a coll -> 'a list

(define-interface set^
  (export union				; 'a sets -> 'a set
	  intersection			; 'a sets -> 'a set
	  difference))			; 'a sets -> 'a set

(define-interface map^
  (export keys				; ('key,_) map -> ('key) list
	  values))			; (_,'value) map -> ('value) list

(define-interface bag^
  (export occurences))			; 'a bag -> int

(define-structure base-collection 
  (export new-order order? order-= order-< 
	  make-collection collection-order collection-value collection?)
  (open scheme match)
  (begin 
    (define-const-structure (order = <))
    (define new-order make-order)
    (define-const-structure (collection order value))
    ))


;; @@ List implementations of different collections 

(define-structure list-set (compound-interface collection^ set^)
  (open scheme match srfi-2 base-collection (with-prefix srfi-1 l:))
  (files list-set))

;(define-structure list-map (compound-interface collection^ map^)
;  (open scheme match base-collection (with-prefix list-set s:))
;  (files list-map))

;(define-structure list-bag (compound-interface collection^ bag^)
;  (open scheme srfi-2 (with-prefix srfi-1 l:) base-collection
;	(with-prefix list-map m:))
;  (files list-bag))

(define-module (make-map set-impl)
  (structure (compound-interface collection^ map^)
	     (open scheme match base-collection (with-prefix set-impl s:))
	     (files map)))

(define-module (make-bag map-impl)
  (structure (compound-interface collection^ bag^)
	     (open scheme base-collection (with-prefix map-impl m:))
	     (files bag)))

(def map (make-map list-set))
(def bag (make-bag map))

;; @ Inductive Graphs

(define-interface graph^
  (export new-context make-context
	  context-preds
	  context-node
	  context-label
	  context-succs

	  update-Context-label

	  new-order
	  empty

	  put-node get-node rem-node 
	  put-edge get-edge rem-edge

	  successors predecessors
	  nodes
	  fold
	  make-graph
	  to-dot

	  put-context rem-context
	  ))

(define-module (make-graph map)
  (structure graph^
    (open scheme match srfi-8
	  (with-prefix srfi-1 l:)
	  (with-prefix map m:))
    (files graph)))

(def graph (make-graph map))

(define-structure graph-algorithms
  (export reverse
	  depth-first-search
	  breadth-first-search
	  topological-sort
	  )
  (open scheme srfi-8 graph
	(with-prefix srfi-1 l:))
  (files graph-algorithms))

