;; syntax
;; LcExp := Sym |
;;  | (lambda (Var) LcExp)
;;  | (LcExp LcExp)

;; occurs-free Sym -> LcExp -> Bool
;; usage return #t if Sym occurs free in LcExp otherwise #f
(define (occurs-free  var exps)
    (cond ((symbol? exps)
            (eqv? var exps))
        ((eqv? 'lambda (car exps))
          (and 
            (not (eqv? (caadr exps) var))
              (occurs-free var (caddr exps))))
        (else (or 
                (occurs-free var (car exps))
                (occurs-free var (cadr exps))))))

;; test occurs-free
(occurs-free 'x 'x)
(occurs-free 'x 'y)
(occurs-free 'x '(lambda (x) (x y)))
(occurs-free 'x '(lambda (z) (x y)))
(occurs-free 'x '((lambda (z) (z y)) (x y)))
(occurs-free 'x '((lambda (x) (x y)) (z y)))

;; list-length List->Int
;; usage (list-length l) return the length of list
(define (list-length l)
  (cond ((null? l) 0)
    (else (+ 1 (list-length (cdr l))))))

;; my-list-ref List->Int->SExp
(define (my-list-ref l n)
  (cond ((zero? n) (car l))
    (else (my-list-ref (cdr l) (- n 1)))))

(define (remove-first var l)
  (cond ((null? l) '())
    ((eqv? var (car l))
      (cdr l))
    (else (cons (car l) (remove-first n (cdr l))))))

;; subset: Sym->Sym->SList->SList
(define (subset new old slist)
  (cond ((null? slist) '())
    ((pair? (car slist))
      (cons (subset new old (car slist))
        (subset new old (cdr slist))))
    ((eqv? (car slist) old)
      (cons new (subset new old (cdr slist))))
    (else (cons (car slist)
            (subset new old (cdr slist))))))

(subset 1 2 '(1 2 3))
(subset 1 2 '(1 (1 2 3) 2 3))

;; duple SExp->Int->SList
(define (duple var n)
  (cond ((= n 0) '())
        (else (cons var
                    (duple var (- n 1))))))

(duple '1 10)
(duple '(1 2 3) 3)

;; down SList-> SList
(define (down l)
  (cond ((null? l) '())
        (else (cons 
               (list (car l)) 
               (down (cdr l))))))

(down '(1 2 3 4))

(define (r p)
  (list (cadr p) (car p)))

;; invert SList(Pair)->SList(Pair)
(define (invert l)
  (cond ((null? l) '())
	(else (cons (r (car l))
		    (invert (cdr l))))))

(define (line sym l)
  (cond ((null? l) '())
      (else (cons (list sym (car l))
		  (line sym (cdr l))))))

(define (_append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
	    (_append (cdr l1) l2))))

(define (product l1 l2)
  (cond ((null? l1) '())
	(else (_append (line (car l1) l2)
		    (product (cdr l1) l2)))))

(define (filter-in pred l)
  (if (null? l) 
      '()
      (if (pred (car l))
	  (cons (car l)
		(filter-in pred (cdr l)))
	  (filter pred (cdr l)))))

(define (c l)
  (if (pair? l)
      l
      (list l)))

(define (up l)
  (if (null? l)
      '()
      (append (c (car l))
	      (up (cdr l)))))
(define (every? pred l)
  (if (null? l)
      #t
      (and (pred (car l)) (every? pred (cdr l)))))

(define (flat? l)
  (every? (lambda (x)
	    (symbol? x))
	  l))

(define (flat-s l)
  (if (symbol? l)
      (list l)
      (flat l)))

(define (flat l)
  (if (null? l)
      '()
      (append (flat-s (car l))
	      (flat (cdr l)))))

(define (s-swapper one other var)
  (cond ((eq? one var)
	 other)
	((eq? other var)
	 one)
	(else var)))

(define (swapper one other l)
  (cond ((null? l) '())
	((pair? (car l))
	 (cons (swapper one other (car l))
	       (swapper one other (cdr l))))
	(else (cons (s-swapper one other (car l))
		    (swapper one other (cdr l)))))) 

(define (merge l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	((<= (car l1) (car l2))
	 (cons (car l1)
	       (merge (cdr l1) l2)))
	(else (cons (car l2)
		    (merge l1 (cdr l2))))))

(define (part loi)
  (list (filter (curry > (car loi)) loi)
	(filter (curry = (car loi)) loi)
	(filter (curry < (car loi)) loi)))

(define (_sort loi)
  (if (null? loi)
      loi
      (append 
       (_sort (car (part loi)))
       (cadr (part loi))
       (_sort (caddr (part loi))))))

(define (_path var bst p)
  (cond ((null? bst) #f)
	((eq? (car bst) var) p)
	(else (let ((l (_path var (cadr bst) (append p (list 'left))))
		    (r (_path var (caddr bst) (append p (list 'right)))))
		(cond ((not (eq? l #f)) l)
		      ((not (eq? r #f)) r)
		      (else #f))))))

(define (path var bst)
  (_path var bst '()))

(path 26 '(14 (7 () (12 () ()))
	      (26 (20 (17 () ())
		      ())
		  (31 () ()))))
