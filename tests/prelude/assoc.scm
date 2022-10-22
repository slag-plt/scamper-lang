(define inventory (list (pair "apples" 5) (pair "bananas" 2) (pair "oranges" 8)))

inventory

(assoc-key? "apples" inventory)

(assoc-key? "grapes" inventory)

(assoc-ref "apples" inventory)

(assoc-ref "bananas" inventory)

(assoc-ref "oranges" inventory)

(define updated-inventory (assoc-set "apples" 3 inventory))

updated-inventory

(assoc-ref "apples" updated-inventory)

(assoc-ref "bananas" updated-inventory)

(assoc-ref "oranges" updated-inventory)
