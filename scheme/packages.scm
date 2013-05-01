
;; @ Structures for the AST
;; There's got to be a better way to expose all the structure procedures
(define-interface ast-interface
  (export Scheme-attr 			; generic attr getter
	  Define? make-Define		; Define
	  Define-var Define-body Define-attr 
	  Define-1 Define-2 Define-3
	  Rec? make-Rec			; Rec
	  Rec-bindings Rec-body Rec-attr 
	  Rec-1 Rec-2 Rec-3
	  Set? make-Set			; Set
	  Set-lhs Set-rhs Set-attr 
	  Set-1 Set-2 Set-3
	  Lambda? make-Lambda		; Lambda 
	  Lambda-parameters Lambda-body Lambda-attr 
	  Lambda-1 Lambda-2 Lambda-3
	  Begin? make-Begin		; Begin
	  Begin-stmt Begin-expr Begin-attr 
	  Begin-1 Begin-2 Begin-3
	  Let? make-Let			; Let
	  Let-var Let-expr Let-body Let-attr
	  Let-1 Let-2 Let-3 Let-4
	  If? make-If			; If 
	  If-cond If-then If-else If-attr
	  If-1 If-2 If-3 If-4
	  App? make-App			; App
	  App-proc App-args App-attr
	  App-1 App-2 App-3 
	  Var? make-Var			; Var 
	  Var-name Var-attr
	  Var-1 Var-2 
	  Value? make-Value		; Value
	  Value-data Value-attr
	  Value-1 Value-2

	  Unspecified False True	; Constants
	  gensym-Var
	  ))

(define-structure ast ast-interface
  (open (modify scheme (hide set!))
	srfi-17 match define-record-types)
  (files ast))


;; @ A-Normalization Algorithm
(define-structure normalize
  (export normalize-term)
  (open scheme srfi-1 match signals)
  (files normalize))


;; @ Parse/Unparse structures
(define-structure parse 
  (export parse unparse)
  (open scheme match srfi-26 srfi-1 srfi-8 ast)
  (files parse))


;; @ Core rewrite strategies
(define-structure core-scheme-strategy strategy-core
  (open scheme match srfi-1 srfi-2 ast)
  (files scheme-strategy))


;; @ Scheme rewrite strategies
(def scheme-strategy (make-strategy core-scheme-strategy))


