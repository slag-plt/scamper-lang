(define factorial
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (factorial (- n 1)))])))

120

(cond
  [#f "shouldn't get here!"]
  [#f "oh no!"])