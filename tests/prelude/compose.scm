(define inc
  (lambda (x) (+ x 1)))

(inc 1)

((compose inc) 1)

((compose inc inc inc inc inc) 1)

(|> 1 inc)

(|> 1 inc inc inc inc inc)

((compose length
          (lambda (l) (filter (lambda (n) (even? n)) l)))
 (range 10))
