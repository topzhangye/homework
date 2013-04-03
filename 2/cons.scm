(define (_cons x y)
  (lambda (f) (f x y)))


(define (first x y)
  x)

(define (second x y)
    y)


(define (_car c)
  (c first))

(define (_cdr c)
  (c second))


(define test (_cons 100 200))

(_car test)
(_cdr test)
