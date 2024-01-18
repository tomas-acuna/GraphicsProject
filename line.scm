#lang racket
(provide line)

(require "table.scm" "pair.scm")

(define (get-function val z)
  (lambda (other-pair)
    (try-pair val z other-pair)))

(define (octant1 table x y z d value end-x a b cz)
  (if (> x end-x)
      table
      (let ([move-up (positive? d)])
        (octant1 (update-table-value table x y (get-function value z)) (add1 x) (if move-up (add1 y) y) (+ z cz) (let ([new-d (+ d a)]) (if move-up (+ new-d b) new-d)) value end-x a b cz))))

(define (octant2 table x y z d value end-y a b cz)
  (if (> y end-y)
      table
      (let ([move-right (negative? d)])
        (octant2 (update-table-value table x y (get-function value z)) (if move-right (add1 x) x) (add1 y) (+ z cz) (let ([new-d (+ d b)]) (if move-right (+ new-d a) new-d)) value end-y a b cz))))

(define (octant8 table x y z d value end-x a b cz)
  (if (> x end-x)
      table
      (let ([move-down (negative? d)])
        (octant8 (update-table-value table x y (get-function value z)) (add1 x) (if move-down (sub1 y) y) (+ z cz) (let ([new-d (+ d a)]) (if move-down (- new-d b) new-d)) value end-x a b cz))))

(define (octant7 table x y z d value end-y a b cz)
  (if (< y end-y)
      table
      (let ([move-right (negative? d)])
        (octant7 (update-table-value table x y (get-function value z)) (if move-right (add1 x) x) (sub1 y) (+ z cz) (let ([new-d (+ d b)]) (if move-right (- new-d a) new-d)) value end-y a b cz))))

(define (no-zero n)
  (if (zero? n)
      1
      n))

(define (line table start-x start-y start-z end-x end-y end-z value)
  (if (< end-x start-x)
      (line table end-x end-y end-z start-x start-y start-z value)
      (let ([x-change (- start-x end-x)]
            [y-change (- end-y start-y)]
            [z-change (- end-z start-z)])
        (let ([a (* y-change 2)]
              [b (* x-change 2)])
          (if (positive? a)
              (if (< a (- b))
                  (octant1 table start-x start-y start-z (+ a x-change) value end-x a b (/ z-change (no-zero x-change)))
                  (octant2 table start-x start-y start-z (+ y-change b) value end-y a b (/ z-change (no-zero y-change))))
              (if (< (- a) (- b))
                  (octant8 table start-x start-y start-z (- a x-change) value end-x a b (/ z-change (no-zero x-change)))
                  (octant7 table start-x start-y start-z (- b y-change) value end-y a b (/ z-change (no-zero y-change)))))))))