(import image)

(define factorial
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (factorial (- n 1)))])))

(factorial 5)

(define red-square (rectangle 15 15 "solid" "red"))

(define type-of
  (lambda (datum)
    (cond
      [(number? datum) "number"]
      [(string? datum) "string"]
      [else "some-other-type"])))

(type-of red-square)

(cond
  [#f "shouldn't get here!"]
  [#f "oh no!"])