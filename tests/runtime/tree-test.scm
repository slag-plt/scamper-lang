(struct leaf (value))

(struct node (left right))

(define tree-size
  (lambda (t)
    (if (leaf? t)
        1
        (+ (tree-size (node-left t))
           (tree-size (node-right t))))))

(define tree-to-list
  (lambda (t)
    (if (leaf? t)
        (list (leaf-value t))
        (append (tree-to-list (node-left t))
                (tree-to-list (node-right t))))))

(define t1
  (node (leaf "a")
        (node (leaf "b")
              (leaf "c"))))

t1

(leaf-value (node-left (node-right t1)))

(tree-size t1)

(tree-to-list t1)