#lang racket
(provide get-table update-table-value table->list get-width-height in-table update-table-values)

(require "tree.scm" "pair.scm" "color.scm")

(struct table (rows height width))

(define (get-table width height value)
  (let ([row (get-tree width value)])
    (let ([rows (get-tree height row)])
      (table rows height width))))

(define (in-table mesa x y)
  (and (>= x 0) (< x (table-width mesa)) (>= y 0) (< y (table-height mesa))))

(define (update-table-value mesa x y function)
  (if (in-table mesa x y)
      (let ([rows (table-rows mesa)]
            [height (table-height mesa)]
            [width (table-width mesa)])
        (let ([row-function (lambda (row) (update-tree-value row width x function))])
          (let ([new-rows (update-tree-value rows height y row-function)])
            (table new-rows height width))))
      mesa))

(define (table->list mesa)
  (let ([rows (table-rows mesa)])
    (let ([col-list (tree->list rows)])
      (let ([value-list-list (reverse (map tree->list col-list))])
        (apply append value-list-list)))))

(define (get-width-height mesa)
  (list (table-width mesa) (table-height mesa)))

(define (update-table-values mesa bottom top left-right-func-function)
  (let ([rows (table-rows mesa)]
        [height (table-height mesa)]
        [width (table-width mesa)])
    (table
     (update-tree-values rows height bottom top (lambda (row y)
                                                  (let ([left-right-func (left-right-func-function y)])
                                                    (let ([left (first left-right-func)]
                                                          [right (second left-right-func)]
                                                          [func (third left-right-func)])
                                                      (update-tree-values row width left right func)))))
     height
     width)))