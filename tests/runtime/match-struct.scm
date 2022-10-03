(struct leaf (value))

(struct node (left right))

(define tree-count
  (lambda (t)
    (match t
      [(leaf _) 1]
      [(node l r) (+ (tree-count l) (tree-count r))])))

(tree-count (leaf "a"))

(tree-count
  (node (leaf "a")
        (node (leaf "b")
              (node (leaf "c")
                    (leaf "d")))))