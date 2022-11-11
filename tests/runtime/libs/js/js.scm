(import js)

(define example
  (js-obj "foo" 0 "bar" 1 "baz" 2))

example

(js-get example "foo")
(js-get example "bar")
(js-get example "baz")

(define str "  hello world!     ")

(js-method str "trim")

(js-eval "1+1")