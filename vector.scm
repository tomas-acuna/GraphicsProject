#lang racket
(provide dot-mult cross-mult normalize)

(define (dot-mult a b)
  (apply + (map * a b)))

(define (cross-mult a b)
  (let ([ax (first a)]
        [ay (second a)]
        [az (third a)]
        [bx (first b)]
        [by (second b)]
        [bz (third b)])
    (list
     (- (* ay bz) (* az by))
     (- (* az bx) (* ax bz))
     (- (* ax by) (* ay bx)))))

(define (get-magnitude vector)
  (sqrt (apply + (map sqr vector))))

(define (normalize vector)
  (let ([magnitude (get-magnitude vector)])
    (map (lambda (n) (/ n magnitude)) vector)))