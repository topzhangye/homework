#lang eopl
(provide equal?? report-unit-test-completed)
(define-syntax equal??
  (syntax-rules ()
    ((_ x y)
     (let ((x_ x) (y_ y))
       (if (equal? x_ y_)
	   (eopl:printf "pass~%")
	   (eopl:printf "~a is not equal ~a ~%" x_ y_))))))


(define report-unit-test-completed
    (lambda (fn_name)
    (eopl:printf "unit test completed ~s~%" fn_name)))


