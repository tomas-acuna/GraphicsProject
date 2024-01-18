#lang racket
(provide triangle)

(require "table.scm" "pair.scm")

(define (get-x-getter start-x start-y end-x end-y)
  (if (= start-y end-y)
      (lambda (y)
        start-x)
      (let ([mult (/ (- end-x start-x) (- end-y start-y))])
        (lambda (y)
          (+ (* mult (- y start-y)) start-x)))))


(define (get-xs-getter bottom-x bottom-y middle-x middle-y top-x top-y)
  (let ([bottom-to-top (get-x-getter bottom-x bottom-y top-x top-y)]
        [bottom-to-middle (get-x-getter bottom-x bottom-y middle-x middle-y)]
        [middle-to-top (get-x-getter middle-x middle-y top-x top-y)])
    (lambda (y)
      (if (< y middle-y)
          (list (bottom-to-top y) (bottom-to-middle y))
          (list (bottom-to-top y) (middle-to-top y))))))

(define (no-zero n)
  (if (zero? n)
      1
      n))

(define (triangle-helper table bottom-x bottom-y bottom-z middle-x middle-y middle-z top-x top-y top-z val)
  (let ([xs-getter (get-xs-getter bottom-x bottom-y middle-x middle-y top-x top-y)]
        [zs-getter (get-xs-getter bottom-z bottom-y middle-z middle-y top-z top-y)])
    (update-table-values table bottom-y top-y (lambda (y)
                                                (let ([left-right (xs-getter y)]
                                                      [front-back (zs-getter y)])
                                                  (let ([left (first left-right)]
                                                        [right (second left-right)]
                                                        [front (first front-back)]
                                                        [back (second front-back)])
                                                    (list left right (let ([slope (/ (- back front) (no-zero (- right left)))])
                                                                       (lambda (value place)
                                                                         (try-pair val (+ front (* slope (- place left))) value))))))))))

(define (triangle table x0 y0 z0 x1 y1 z1 x2 y2 z2 value)
  (cond [(<= y0 y1 y2)
         (triangle-helper table x0 y0 z0 x1 y1 z1 x2 y2 z2 value)]
        [(<= y0 y2 y1)
         (triangle-helper table x0 y0 z0 x2 y2 z2 x1 y1 z1 value)]
        [(<= y1 y0 y2)
         (triangle-helper table x1 y1 z1 x0 y0 z0 x2 y2 z2 value)]
        [(<= y1 y2 y0)
         (triangle-helper table x1 y1 z1 x2 y2 z2 x0 y0 z0 value)]
        [(<= y2 y0 y1)
         (triangle-helper table x2 y2 z2 x0 y0 z0 x1 y1 z1 value)]
        [(<= y2 y1 y0)
         (triangle-helper table x2 y2 z2 x1 y1 z1 x0 y0 z0 value)]
        [else
         table]))