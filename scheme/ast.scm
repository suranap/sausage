
;; @ Core Scheme AST 

(define-structure (Define var body attr))
(define-structure (Set lhs rhs attr))
(define-structure (Lambda parameters body attr))
(define-structure (Begin stmt expr attr))
(define-structure (Let var expr body attr))
(define-structure (Rec bindings body attr))
(define-structure (If cond then else attr))
(define-structure (App proc args attr))
(define-structure (Var name attr))
(define-structure (Value data attr))

;; @ Constants

(define Unspecified (make-Value 'unspecified '()))
(define False (make-Value #f '()))
(define True (make-Value #t '()))

;; @ Type-safe constructors

;; Use these constructors to ensure at run-time that the syntax trees
;; are type correct. These replace make-* for the AST.

;(define (new-Define var body . attrs)
;  )

;; Generic attribute getter/setter
(define (Scheme-attr node)
  (cond
   ((Define? node) (Define-attr node))
   ((Set? node) (Set-attr node))
   ((Lambda? node) (Lambda-attr node))
   ((Begin? node) (Begin-attr node))
   ((Let? node) (Let-attr node))
   ((Rec? node) (Rec-attr node))
   ((If? node) (If-attr node))
   ((App? node) (App-attr node))
   ((Var? node) (Var-attr node))
   ((Value? node) (Value-attr node))))


;; @@ Generic setters (using srfi-17)

(set! (setter Define-var) set-Define-var!)
(set! (setter Define-body) set-Define-body!)
(set! (setter Define-attr) set-Define-attr!)
(set! (setter Set-lhs) set-Set-lhs!)
(set! (setter Set-rhs) set-Set-rhs!)
(set! (setter Set-attr) set-Set-attr!)
(set! (setter Lambda-parameters) set-Lambda-parameters!)
(set! (setter Lambda-body) set-Lambda-body!)
(set! (setter Lambda-attr) set-Lambda-attr!)
(set! (setter Begin-stmt) set-Begin-stmt!)
(set! (setter Begin-expr) set-Begin-expr!)
(set! (setter Begin-attr) set-Begin-attr!)
(set! (setter Let-var) set-Let-var!)
(set! (setter Let-expr) set-Let-expr!)
(set! (setter Let-body) set-Let-body!)
(set! (setter Let-attr) set-Let-attr!)
(set! (setter Rec-bindings) set-Rec-bindings!)
(set! (setter Rec-body) set-Rec-body!)
(set! (setter Rec-attr) set-Rec-attr!)
(set! (setter If-cond) set-If-cond!)
(set! (setter If-then) set-If-then!)
(set! (setter If-else) set-If-else!)
(set! (setter If-attr) set-If-attr!)
(set! (setter App-proc) set-App-proc!)
(set! (setter App-args) set-App-args!)
(set! (setter App-attr) set-App-attr!)
(set! (setter Var-name) set-Var-name!)
(set! (setter Var-attr) set-Var-attr!)
(set! (setter Value-data) set-Value-data!)
(set! (setter Value-attr) set-Value-attr!)

;; @@ Record disclosers

(define-record-discloser :Define 
  (match-lambda (($ Define V B) (list 'define V B))))

(define-record-discloser :Set 
  (match-lambda (($ Set L R) (list 'set L R))))

(define-record-discloser :Lambda 
  (match-lambda (($ Lambda P* B) (list 'lambda P* B))))

(define-record-discloser :Begin 
  (match-lambda (($ Begin S E) (list 'begin S E))))

(define-record-discloser :Let 
  (match-lambda (($ Let V E B) (list 'let V E B))))

(define-record-discloser :Rec 
  (match-lambda (($ Rec B+ B) (list 'rec B+ B))))

(define-record-discloser :If 
  (match-lambda (($ If E C A) (list 'if E C A))))

(define-record-discloser :App 
  (match-lambda (($ App F A*) (list 'app F A*))))

(define-record-discloser :Var 
  (match-lambda (($ Var N) (list 'var n))))

(define-record-discloser :Value 
  (match-lambda (($ Value D) (list 'value D))))

;; @ Helpers 

(define *count* 0)
(define (gensym-Var . basename)
  (let ((basename (if (pair? basename) 
		      (symbol->string (car basename)) 
		      "gen")))
    (set! *count* (+ 1 *count*))
    (make-Var (string->symbol (string-append basename (number->string *count*))) '())))

