
;;
;; A standard library for Scheme48
;;

(define (not x)
  (if x #f #t))

(define (null? obj)
  (if (eqv? obj '()) #t #f))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip func)
  (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1)
  (lambda (arg) (apply func (cons arg1 (list arg)))))

(define (compose f g)
  (lambda (args) (f (apply g args))))

(define zero?
  (curry = 0))

(define positive?
  (curry < 0))

(define negative?
  (curry > 0))

(define (odd? num)
  (= (mod num 2) 1))

(define even?
  (compose not odd?))

(define (foldl func acc lst)
  (if (null? lst)
      acc
      (foldl func (func acc (car lst)) (cdr lst))))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (sum . lst)
  (fold + 0 lst))

(define (product . lst)
  (fold * 1 lst))

(define (and . lst)
  (fold && #t lst))

(define (or . lst)
  (fold || #f lst))

(define (max first . rest)
  (fold (lambda (a b) (if (>= a b) a b)) first rest))

(define (min first . rest)
  (fold (lambda (a b) (if (<= a b) a b)) first rest))

(define (length lst)
  (fold (lambda (a _) (+ 1 a)) 0 lst))

(define (reverse lst)
  (foldl (flip cons) '() lst))

(define (mem-helper pred op)
  (lambda (acc next) (or acc (pred (op next)))))

(define (member obj lst)
  (fold (mem-helper (curry equal? obj) id) #f lst))

(define (memq obj lst)
  (fold (mem-helper (curry eq? obj) id) #f lst))

(define (memv obj lst)
  (fold (mem-helper (curry eqv? obj) id) #f lst))

(define (assq obj alist)
  (fold (mem-helper (curry eq? obj) car) #f alist))

(define (assv obj alist)
  (fold (mem-helper (curry eqv? obj) car) #f alist))

(define (assoc obj alist)
  (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map func lst)
  (foldr (lambda (e a) (cons (func e) a)) '() lst))

(define (filter pred lst)
  (foldr (lambda (e l) (if (pred e) (cons e l) l)) '() lst))

