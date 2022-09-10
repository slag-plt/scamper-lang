; bindings are not dependent on each other
(let
  ([x 1]
   [y 7]
   [z 11])
  (+ x y z))

(let*
  ([x 1]
   [y 7]
   [z 11])
  (+ x y z))

; bindings telescope
(let*
  ([x 1]
   [y (+ x 6)]
   [z (+ y 4)])
  (+ x y z))