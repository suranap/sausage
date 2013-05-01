
;;; partially evaluate a format string

(define evaluate-format-string
  (match-lambda 
   (($ App ($ Var 'format) (($ Value F) As ...))
    (normalize 
     (parse
      (cons 'string-append
	    (letrec next ((Fs (parse-format-string F))
			  (As As))
		    (match (car Fs)
			   ((? string? S) 
			    (cons S (next (cdr Fs) As)))
			   (('directive D)
			    `((format/one ,D ,(car As))
			      ,@(next (cdr Fs) (cdr As))))))))))))

(define format/one-string
  (match-scheme
   (('format/one "S" A) A)))

(define format/one-number
  (match-scheme
   (('format/one "D" A) `(number->string ,A))))

