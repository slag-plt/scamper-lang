(define x 3)

(define y (+ x 2))

(define x -5)

(+ x y)

(define f
  (lambda (x)
    (* x 2)))

(f 3)

(let*
  ([z 10]
   [x (+ z x)]
   [z 100])
  (+ x z))

x