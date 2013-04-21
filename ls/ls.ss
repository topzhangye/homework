(define nil '())

(define (atom? x)
  (and (not (pair? x))
    (not (null? x))))

(define (lat? x)
  (cond ((null? x) #t)
    ((atom? (car x)) (lat? (cdr x)))
    (else #f)))

(define (member? a lat)
  (if (null? lat) #f
    (or (eq? a (car lat))
      (member? a (cdr lat)))))

(define (rember a lat)
  (cond ((null? lat) nil)
    ((eq? a (car lat)) (cdr lat))
    (else (cons (car lat) (rember a (cdr lat))))))

(define (firsts lat)
  (cond ((null? lat) nil)
    (else (cons (car (car lat)) (firsts (cdr lat))))))

(define (insertR new old lat)
  (cond ((null? lat) nil)
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (cond ((null? lat) nil)
    ((eq? old (car lat)) (cons new (cons old (cdr lat))))
    (else (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond ((null? lat) nil)
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))

(define (multirember a lat)
  (cond ((null? lat) nil)
    ((eq? a (car lat)) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))

(define (multiinsertL new old lat)
  (cond ((null? lat) nil)
    ((eq? old (car lat))
      (cons new 
        (cons old (multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond ((null? lat) nil)
    ((eq? old (car lat))
      (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))

;; number games
(define zero 0)

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (add a b)
  (cond ((zero? b) a)
    (else (add1 (add a (sub1 b))))))

(define (sub a b)
  (cond ((zero? b) a)
    (else (sub1 (sub a (sub1 b))))))

(define (addtup lat)
  (cond ((null? lat) zero)
    (else (add (car lat) (addtup (cdr lat))))))

(define (mul a b)
  (cond ((zero? b) zero)
    (else (add a (mul a (sub1 b))))))

(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (add (car tup1)
                     (car tup2))
                (tup+ (cdr tup1) (cdr tup2))))))

(define (n> a b)
  (cond ((zero? a) #f)
    ((zero? b) #t)
    (else (n> (sub1 a) (sub1 b)))))

(define (n< a b)
  (cond ((zero? b) #f)
    ((zero? a) #t)
    (else (n< (sub1 a) (sub1 b)))))

(define (n= a b)
  (cond ((n< a b) #f)
    ((n> a b) #f)
    (else #t)))

(define (pow a b)
  (cond ((zero? b) 1)
    (else (mul a 
            (pow a (sub1 b))))))

(define (div a b)
  (cond ((n< a b) 0)
    (else (add1 (div (sub a b) b)))))

(define (len lat)
  (cond ((null? lat) 0)
    (else (add1 (len (cdr lat))))))


(define (pick n lat)
  (cond ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond ((zero? (sub1 n)) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond ((null? lat) nil)
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat) 
            (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond ((null? lat) nil)
    ((number? (car lat)) 
      (cons (car lat) (all-nums (cdr lat))))
    (else (all-nums (cdr lat)))))

(define (eqan? a b)
  (cond ((and (number? a) (number? b)) (n= a b))
    ((or (number? a) (number? b)) #f)
    (else (eq? a b))))

(define (occur a lat)
  (cond ((null? lat) 0)
    ((eq? a (car lat))
      (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat)))))

(define (one? n)
  (n= n 1))

(define (rember* a l)
  (cond ((null? l) nil)
    ((pair? (car l))
      (cons (rember* a (car l))
        (rember* a (cdr l))))
    ((eq? a (car l)) (rember* a (cdr l)))
    (else (cons (car l) (rember* a (cdr l))))))

(define (insertL* new old l)
  (cond ((null? l) nil)
    ((pair? (car l))
      (cons (insertL* new old (car l))
        (insertL* new old (cdr l))))
    ((eq? old (car l))
      (cons new
        (cons old 
          (insertL* new old (cdr l)))))
    (else
      (cons (car l)
        (insertL* new old (cdr l))))))

(define (insertR* new old l)
  (cond ((null? l) '())
    ((pair? (car l))
      (cons (insertR* new old (car l))
        (insertR* new old (cdr l))))
    ((eq? old (car l))
      (cons old
        (cons new
          (insertR* new old (cdr l)))))
    (else
      (cons (car l)
        (insertR* new old (cdr l))))))

(define (occur* a l)
  (cond ((null? l) 0)
    ((pair? (car l))
      (add 
        (occur* a (car l))
        (occur* a (cdr l))))
    ((eq? a (car l))
      (add1 (occur* a (cdr l))))
    (else
      (occur* a (cdr l)))))

(define (member* a l)
  (cond ((null? l) #f)
    ((pair? (car l))
      (or (member* a (car l))
        (member* a (cdr l))))
    ((eq? a (car l)) #t)
    (else (member* a (cdr l)))))

(define (leftmost l)
  (cond ((atom? (car l)) (car l))
    (else (leftmost (cdr l)))))

(define (eqlist? a b)
  (cond ((and (null? a) (null? b)) #t)
    ((or (null? a) (null? b)) #f)
    ((and (atom? (car a)) (atom? (car b)))
      (and (eq?  (car a) (car b)) 
        (eqlist? (cdr a) (cdr b))))
    ((or (atom? (car a)) (atom? (car b))) #f)
    (else (and (eqlist? (car a) (car b))
      (eqlist? (cdr a) (cdr b))))))

      
