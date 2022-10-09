(define max-value 5)
(define num-tests 1000)

(|> (make-list num-tests max-value)
    (lambda (l) (map random l))
    (lambda (l) (map (lambda (x) (and (>= x 0) (< x max-value))) l))
    (lambda (l) (reduce (lambda (b1 b2) (and b1 b2)) l))
    )