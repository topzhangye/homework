(define (id x) x)

(define (compose . fun-list)
    (foldl 
        (lambda (x y) 
            (lambda (t) (y (x t))))
        id
        fun-list))
