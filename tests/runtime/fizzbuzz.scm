(define fizzbuzz
  (lambda (n)
    (cond
      [(and (zero? (modulo n 3)) (zero? (modulo n 5))) "fizzbuzz"]
      [(zero? (modulo n 3)) "fizz"]
      [(zero? (modulo n 5)) "buzz"]
      [#t (number->string n)])))

(fizzbuzz 1)
(fizzbuzz 2)
(fizzbuzz 3)
(fizzbuzz 4)
(fizzbuzz 5)
(fizzbuzz 6)
(fizzbuzz 7)
(fizzbuzz 8)
(fizzbuzz 9)
(fizzbuzz 10)
(fizzbuzz 11)
(fizzbuzz 12)
(fizzbuzz 13)
(fizzbuzz 14)
(fizzbuzz 15)