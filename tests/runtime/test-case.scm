(test-case "simple equality success"
  equal? 1 1)

(test-case "simple equality failure"
  equal? 1 2)

(test-case "simple equality failure with computation"
  = (/ 10 2) (+ 1 2))

(test-case "epsilon equality success"
  (=-eps 0.001) 3 3.00001)

(test-case "epsilon equality failure"
  (=-eps 0.001) 3 5)