(define inc
  (lambda (x) (+ x 1)))

(define l (list 1 2 3 4 5 6 7 8 9 10))

(map inc l)

(map (lambda (p) (car p))
  (list (cons "a" "b") (cons "c" "d") (cons "e" "f")))

(map inc null)

(map - (list 3 5 7 1 9)
       (list 1 2 3 0 13))

(map (lambda (x y z) (if x y z))
     (list #t #f #t)
     (list "yes" "no" "maybe")
     (list "y" "n" "m"))

(filter
  (lambda (v)
    (= (remainder v 2) 0))
  l)

(filter
  (lambda (v)
    (>= v 5))
  l)

(filter
  (lambda (v)
    (< v 5))
  l)

(filter (lambda (v) (> v 20)) l)

(filter (lambda (v) #t) null)

(fold + 0 l)
(reduce + l)