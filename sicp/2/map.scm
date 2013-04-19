#lang racket

(define nil '())

(define (_append first second)
  (foldr cons second first))

(define (_map op seq)
  (foldr 
    (lambda (x y) 
      (append (list (op x)) y ))
    nil 
    seq))
