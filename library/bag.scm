
;; @ Collection interface 

(define new-order m:new-order)
(define new m:new)
(define size m:size)
(define member m:member)

(define (insert elem bag)
  (cond
   ((member elem bag) => 
    (lambda (count)
      (m:insert elem (+ count 1) bag)))
   (else (m:insert elem 1 bag))))

(define (remove elem bag)
  (cond
   ((member elem bag) => 
    (lambda (count)
      (if (> count 1) 
	  (m:insert elem (- count 1) bag)
	  (m:remove elem bag))))
   (else bag)))

;; if there are N elem's in the bag, this will call f N separate times. 
(define (fold f i bag) 
  (m:fold (lambda (elem count i)
	    (let loop ((c count) (i i))
	      (if (> c 0) (loop (- c 1) (f elem i)) i)))
	  i bag))

;; if there are N elem's in the bag, this will include it N times in the list
(define (elements bag)
  (fold cons '() bag))

;; export the bag interface
(define (occurences bag)
  (fold (lambda (elem count) (+ count 1)) 0 bag))

