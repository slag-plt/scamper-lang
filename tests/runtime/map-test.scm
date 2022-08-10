(define inc
  (lambda (x) (+ x 1)))

(define l (list 1 2 3 4 5 6 7 8 9 10))

(map inc l)

(map (lambda (p) (car p)) (list (cons "a" "b") (cons "c" "d") (cons "e" "f")))