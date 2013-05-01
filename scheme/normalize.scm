
;; @ A-Normalization 

;; @ A-Normalization Algorithm

(define (normalize-term M) (normalize M (lambda (x) x)))

(define (normalize M k)
  (match M
	 (('define V M1)        (k `(define ,V ,(normalize-term M1))))

	 (('set! V M1)          (normalize-name M1 (lambda (t) (k `(set! ,V ,t)))))

	 (('lambda params body) (k `(lambda ,params ,(normalize-term body))))
					; do I need to remove begin?
	 (('begin . M*)         (k `(begin ,@(map normalize-term M*))))

 	 ((('lambda frmls body) args ...) 
 	  (normalize (apply-lambda frmls body args) k))

 	 ((('case-lambda . entry-points) . args)
	  (let ((ep (select-entry-point entry-points (length args))))
	    (normalize (apply-lambda (car ep) (cadr ep) args) k)))


	 (('case-lambda . lambda-terms)	; experimental
	  (k (cons 'case-lambda
		   (map (lambda (lterm)
			  (match-let (((params body) lterm)) (list params (normalize-term body))))
			lambda-terms))))

	 (('letrec ((vars defs) ...) body)
	  (k `(letrec ,(zip vars (map normalize-term defs)) ,(normalize-term body))))

	 (('let ((x (and M1 (not (? Value?))))) M2)
                                (normalize M1 (lambda (N1) `(let ((,x ,N1)) ,(normalize M2 k)))))
	 (('let ((x (and M1 (? Value?)))) M2)
                                `(let ((,x ,M1)) ,(normalize M2 k)))
	 (('if M1 M2 M3)        (normalize-name M1 (lambda (t) (k `(if ,t ,(normalize-term M2) ,(normalize-term M3))))))
	 (('if M1 M2)           (normalize-name M1 (lambda (t) (k `(if ,t ,(normalize-term M2) 'unspecified)))))
	 ((and e ('quote v))    (k e))
	 (((and Fn (not (? primitive?))) . M*)
	                        (if (PrimOp? Fn)
				    (normalize-name* M* (lambda (t*) (k `(,Fn . ,t*))))
				    (normalize-name Fn (lambda (t) (normalize-name* M* (lambda (t*) (k `(,t . ,t*))))))))
	 (V                     (k V))))

(define (normalize-name M k)
  (normalize M (lambda (N) (if (Value? N) (k N) (let ((t (newvar))) `(let ((,t ,N)) ,(k t)))))))

(define (normalize-name* M* k)
  (cond 
   ((null? M*) (k '()))
;   ((symbol? (car M*)) (normalize-name* (cdr M*) (lambda (t*) (k `(,(car M*) . ,t*)))))
   (else (normalize-name (car M*) (lambda (t) (normalize-name* (cdr M*) (lambda (t*) (k `(,t . ,t*)))))))))

(match-define (newvar currvar reset)
  (let ((counter 0)
	(name '*))
    (list
     (lambda () (begin (set! counter (+ counter 1))
		       (set! name (string->symbol (string-append "v_" (number->string counter))))
		       name))
     (lambda () name)
     (lambda () (set! counter 0)))))

(define PrimOp? symbol?)


;; misc
(define (apply-lambda frmls body args)
  (fold-right (lambda (h t) `(let (,h) ,t)) 
	      body 
	      (zip (normalize-formals frmls) 
		   (if (n-ary? frmls)
		       (append (take args (number-of-required-args frmls))
			       (cons 'list (drop args (number-of-required-args frmls))))
		       args))))

;; find the first entry point in case-lambda to match the arguments
(define (select-entry-point eps nargs)
  (if (null? eps)
      (error "No branch in case-lambda can be applied to arguments.")
      (let ((nreq (number-of-required-args (caar eps)))
	    (is-nary (n-ary? (caar eps))))
	(if (or (= nreq nargs)
		(and (< nreq nargs) is-nary))
	    (car eps)
	    (select-entry-point (cdr eps) nargs)))))

;; am I missing something?
(define (Value? x)
  (or (boolean? x)
      (char? x)
      (number? x)
      (string? x)
      (null? x)
      (and (pair? x) (eq? 'quote (car x)))))


;; @ Auxillary

;; @@ Primitive procedures

(define (primitive? f)
  (and (symbol? f)
       (eq? (string-ref (symbol->string f) 0) #\%)))


;; @@ N-ary helpers
(define (number-of-required-args formals)
  (do ((l formals (cdr l))
       (i 0 (+ i 1)))
      ((not (pair? l)) i)))
    
(define (n-ary? formals)
  (cond ((null? formals) #f)
	((pair? formals) (n-ary? (cdr formals)))
	(else #t)))
    
(define (normalize-formals formals)
  (cond ((null? formals)
	 '())
	((pair? formals)
	 (cons (car formals)
	       (normalize-formals (cdr formals))))
	(else
	 (list formals))))


