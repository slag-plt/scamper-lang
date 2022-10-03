(match 5
  [1 "fail"]
  [5 "numbers"])

(match "baz"
  ["foo" "fail"]
  ["bar" "fail"]
  ["baz" "strings"]
  ["boop" "fail"]) 

(match #\q
  [#\a "fail"]
  [#\q "chars"]
  [#\z "fail"])

(match #t
  [#f "fail"]
  [#t "bools"])

(match null
  [null "null"])

(match (list "lists" "a" "b")
  [null "fail"]
  [(cons head _) head])