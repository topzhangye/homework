#lang racket

(define (square x)
    (* x x))

(define (inc x)
    (+ x 1))

(define (compose f g)
    (lambda (x)
          (f (g x))))

(define (repeat f n)
    (if (= n 0)
            (lambda (x) x)
                  (compose (repeat f (- n 1)) f)))
