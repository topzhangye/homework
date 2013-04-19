;; interface
(provide in-S?)

;;in-S?: N->Bool
;;usage: (in-S? n) = #t if n is in S, else #f
(define (in-S? n)
  (if (zero? n) #t
      (if (>= (- n 3) 0)
	  (in-S? (- n 3))
	  #f)))
