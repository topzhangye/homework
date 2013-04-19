#lang racket
(define nil '())


(define (atom? sep)
  (and (not (pair? sep))
       (not (null? sep))))

(define (seq low high step)
  (if (> low high)
    nil
    (cons low (seq (+ low step)
                   high
                   step))))

(define (fold-left op init seq)
  (if (null? seq)
      init
      (fold-left op (op init (car seq)) (cdr seq))))

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)  
	  (fold-right op init (cdr seq)))))

(define (sum l)
  (foldl + 0 l))

(define (seq-100)
  (seq 1 100 1))

;(foldl eq foldr op commutation law)

(define (reverse-right seq)
  (fold-right (lambda (x y) (append y (list x)))
	      nil
	      seq))

(define (reverse-left seq)
  (fold-left (lambda (x y) (append (list y) x))
            nil
            seq))

(define (zy-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (zy-append (cdr list1) list2))))

(define (last-pair sep)
  (if (null? (cdr sep))
      sep
      (last-pair (cdr sep))))

(define (zy-reverse seq)
  (if (null? seq)
      nil
      (append (zy-reverse (cdr seq)) (list (car seq)))))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

