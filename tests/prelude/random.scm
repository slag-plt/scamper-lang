(define max-value 5)
(define num-tests 100)

(|> (make-list num-tests max-value)
    (lambda (l) (map random l))
    (lambda (l) (map (lambda (x) (and (>= x 0) (< x max-value))) l))
    (lambda (l) (reduce (lambda (b1 b2) (and b1 b2)) l))
    )

(define dice
  (lambda ()
    (+ 1 (random 6))))

(define make-dice-rolls
  (lambda (n)
    (if (zero? n)
        null
        (cons (dice) (make-dice-rolls (- n 1))))))

(|> (make-dice-rolls num-tests)
    (lambda (l) (map (lambda (x) (and (>= x 1) (<= x 6))) l))
    (lambda (l) (reduce (lambda (b1 b2) (and b1 b2)) l))
    )
