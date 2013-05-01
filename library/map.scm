
;; @ Maps based on sets
(define-const-structure (entry key value))

(define (new-order = <)
  (s:new-order (lambda (x y) (= (entry-key x) (entry-key y)))
	       (lambda (x y) (< (entry-key x) (entry-key y)))))

;; @@ Map interface 

(define (keys m) 
  (map entry-key (s:elements m)))

(define (values m) 
  (map entry-value (s:elements m)))


;; @@ Collection interface

(define new s:new)
(define size s:size)

(define (member key map) 
  (let ((e (s:member (make-entry key '()) map)))
    (and e (entry-value e))))

(define (insert key value map)
  (s:insert (make-entry key value) map))

(define (remove key map) 
  (s:remove (make-entry key '()) map))

;; ('key * 'value * 'init -> 'init) * 'init * map -> 'init
(define (fold f i map) 
  (s:fold (lambda (e i)
	    (f (entry-key e) (entry-value e) i))
	  i map))

;; elements :: map('a,'b) -> [('a . 'b)]
(define (elements m)
  (map cons (keys m) (values m)))

