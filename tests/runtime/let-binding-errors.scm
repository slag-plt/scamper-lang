; let bindings telescope
(let
  ([x1 1]
   [y1 (+ x1 6)])
  (+ x1 y1))

; let bindings refer to future bindings

(let
  ([x2 y2]
   [y2 5])
  (+ x2 y2))

(let*
  ([x3 y3]
   [y3 5])
  (+ x3 y3))