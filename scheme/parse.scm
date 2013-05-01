
;; @ Parse/Unparse Core Scheme

;; Utilities to parse a core scheme expression into an abstract syntax
;; tree, plus an unparser (or writer). 

;; @ Parse 

(define parse
  (match-lambda
   (('define V B)      (make-Define (parse V) (parse B) '()))
   (('set! V V2)       (make-Set (parse V) (parse V2) '()))
   (('lambda P* B)     (make-Lambda (parse-parameters P*) (parse B) '()))
   (('begin . E*)      (reduce-right (cut make-Begin <> <> '()) Unspecified (map parse E*)))
   (('let ((V E)) B)   (make-Let (parse V) (parse E) (parse B) '()))
   (('letrec (B+ ...) B) (make-Rec (receive (vars defs) (unzip2 B+) 
					    (zip (map parse vars) (map parse defs)))
				   (parse B) '()))
   (('if C T . E)      (make-If (parse C) (parse T) (if (null? E) Unspecified (parse E)) '()))
   ((and e ('quote D)) (make-Value e '()))
   ((F . A*)           (make-App (parse F) (map parse A*) '()))
   (V                  (if (symbol? V) (make-Var V '()) (make-Value V '())))))


;; @ Unparse 
(define unparse
  (match-lambda 
   (($ Define V B)  `(define ,(unparse V) ,(unparse B)))
   (($ Set V V2)    `(set! ,(unparse V) ,(unparse V2)))
   (($ Lambda P* B) `(lambda ,(unparse-parameters P*) ,(unparse B)))
   (($ Begin S E)   `(begin ,(unparse S) ,(unparse E)))
   (($ Let V E B)   `(let ((,(unparse V) ,(unparse E))) ,(unparse B)))
   (($ Rec B+ B)    `(letrec ,(map (lambda (def) (list (unparse (car def)) (unparse (cadr def)))) B+) 
		       ,(unparse B)))
   (($ If C T E)    `(if ,(unparse C) ,(unparse T) ,(unparse E)))
   (($ App F A*)    `(,(unparse F) ,@(map unparse A*)))
   (($ Var N)       N)
   (($ Value D)     D)))


;; @ Utilities

;; TODO: handle n-ary parameters 
(define (parse-parameters params)
  (map parse (normalize-formals params)))  ;; still wrong
      

;; TODO: handle n-ary parameters 
(define (unparse-parameters params)
  (map unparse params))

(define (normalize-formals formals)
  (cond ((null? formals)
	 '())
	((pair? formals)
	 (cons (car formals)
	       (normalize-formals (cdr formals))))
	(else
	 (list formals))))

