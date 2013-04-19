#lang racket
(define (say . messages)
  (for-each (lambda (x)
             (display x)
             (newline)) 
            messages))
