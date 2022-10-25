(define x 10)

(define f1
  (lambda (y) (+ x y)))

(f1 20)

(define f2
  (let ([x 100])
    (lambda (y) (+ x y))))

(f2 20)

(define f3
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (+ x y z)))))

(((f3 11) 3) 7)

(define f4
  (let* ([x 51]
         [f (lambda (y) (+ x y))]
         [g (lambda (x) (+ (f x) 1))])
    g))
  
(f4 100)