#lang racket
(provide get-tree get-tree-value update-tree-value set-tree-value update-tree-values set-tree-values tree->list)

(struct tree (value left right))

(define (half n)
  (quotient n 2))

(define (get-tree size value)
  (if (zero? size)
      null
      (let ([middle (half size)])
        (let ([left-size middle]
              [right-size (- size (add1 middle))])
          (let ([left-tree (get-tree left-size value)]
                [right-tree (get-tree right-size value)])
            (tree value left-tree right-tree))))))

(define (get-tree-value arbol size n)
  (let ([middle (half size)])
    (if (= n middle)
        (tree-value arbol)
        (if (< n middle)
            (let ([new-tree (tree-left arbol)]
                  [new-size middle])
              (get-tree-value new-tree new-size n))
            (let ([new-tree (tree-right arbol)]
                  [new-size (- size (add1 middle))]
                  [new-n (- n (add1 middle))])
              (get-tree-value new-tree new-size new-n))))))

(define (update-tree-value arbol size n function)
  (let ([value (tree-value arbol)] 
        [left-tree (tree-left arbol)]
        [right-tree (tree-right arbol)]
        [middle (half size)])
    (if (= n middle)
        (tree (function value) left-tree right-tree)
        (if (< n middle)
            (let ([left-size middle])
              (let ([new-left-tree (update-tree-value left-tree left-size n function)])
                (tree value new-left-tree right-tree)))
            (let ([right-size (- size (add1 middle))]
                  [new-n (- n (add1 middle))])
              (let ([new-right-tree (update-tree-value right-tree right-size new-n function)])
                (tree value left-tree new-right-tree)))))))

(define (set-tree-value arbol size n new-value)
  (update-tree-value arbol size n (lambda (value) new-value)))

(define (update-tree-values-helper arbol size lower upper function behind)
  (if (zero? size)
      null
      (let ([value (tree-value arbol)] 
            [left-tree (tree-left arbol)]
            [right-tree (tree-right arbol)]
            [middle (half size)])
        (tree
         (if (and (<= lower middle) (< middle upper))
             (function value (+ behind middle))
             value)
         (if (< lower middle)
             (let ([left-size middle])
               (update-tree-values-helper left-tree left-size lower upper function behind))
             left-tree)
         (let ([middle+1 (add1 middle)])
           (if (< middle+1 upper)
               (let ([right-size (- size middle+1)]
                     [new-lower (- lower middle+1)]
                     [new-upper (- upper middle+1)]
                     [new-behind (+ behind middle+1)])
                 (update-tree-values-helper right-tree right-size new-lower new-upper function new-behind))
               right-tree))))))

(define (update-tree-values arbol size a b function)
  (if (< a b)
      (update-tree-values-helper arbol size a b function 0)
      (update-tree-values-helper arbol size b a function 0)))

(define (set-tree-values arbol size lower upper new-value)
  (update-tree-values arbol size lower upper (lambda (value position) new-value)))

(define (tree->list arbol)
  (if (null? arbol)
      null
      (let ([value (tree-value arbol)]
            [left-list (tree->list (tree-left arbol))]
            [right-list (tree->list (tree-right arbol))])
        (append left-list (cons value right-list)))))