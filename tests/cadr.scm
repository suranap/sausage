

(define count 1)
(define (gensym-Var)
  (begin (set! count (+ count 1))
	 (make-Var (string->symbol (string-append "CR" (number->string count))) '())))


(define car-name-alist '((caar car) (cdar cdr)  (caaar caar)
			 (cdaar cdar)
			 (cadar cadr)
			 (cddar cddr)
			 (caaaar caaar)
			 (cdaaar cdaar)
			 (cadaar cadar)
			 (cddaar cddar)
			 (caadar caadr)
			 (cdadar cdadr)
			 (caddar caddr)
			 (cdddar cdddr)))

(define cdr-name-alist '((cadr car) (cddr cdr)
			  (caadr caar)
			  (cdadr cdar)
			  (caddr cadr)
			  (cdddr cddr)
			  (caaadr caaar)
			  (cdaadr cdaar)
			  (cadadr cadar)
			  (cddadr cddar)
			  (caaddr caadr)
			  (cdaddr cdadr)
			  (cadddr caddr)
			  (cddddr cdddr)))


(define (cdr? V) (assv V cdr-name-alist))
(define (car? V) (assv V car-name-alist))

(define cadr-simplify
  (match-lambda
   (($ Let V ($ App ($ Var (and F (? car?))) (A)) B)
    (let ((T (gensym-Var)))
      (make-Let T (make-App (make-Var 'car '()) (list A) '())
		(make-Let V (make-App (make-Var (cadr (car? F)) '()) (list T) '())
			  B '())
		'())))

   (($ Let V ($ App ($ Var (and F (? cdr?))) (A)) B)
    (let ((T (gensym-Var)))
      (make-Let T (make-App (make-Var 'cdr '()) (list A) '())
		(make-Let V (make-App (make-Var (cadr (cdr? F)) '()) (list T) '())
			  B '())
		'())))))


(define test-rule-1
  (match-lambda
   (($ Let V ($ App ($ Var 'cdar) (A)) B) (display "GOTIT") #f)))
