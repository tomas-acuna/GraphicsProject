#lang racket
(provide get-box get-sphere get-torus)

(require racket/math "connect.scm" "curve.scm" "matrix.scm" "get-matrix.scm" "get-threed-matrix.scm")

(define tau (* pi 2))

(define unit-box '((1 -1 0 1)
                   (1 0 0 1)
                   (0 0 0 1)
                   (0 0 0 1)
                   (0 -1 0 1)
                   (1 -1 0 1)
                   
                   (1 -1 -1 1)
                   (1 0 -1 1)
                   (1 0 0 1)
                   (1 0 0 1)
                   (1 -1 0 1)
                   (1 -1 -1 1)
                   
                   (0 -1 -1 1)
                   (0 0 -1 1)
                   (1 0 -1 1)
                   (1 0 -1 1)
                   (1 -1 -1 1)
                   (0 -1 -1 1)
                   
                   (0 -1 0 1)
                   (0 0 0 1)
                   (0 0 -1 1)
                   (0 0 -1 1)
                   (0 -1 -1 1)
                   (0 -1 0 1)
                   
                   (1 0 0 1)
                   (1 0 -1 1)
                   (0 0 -1 1)
                   (0 0 -1 1)
                   (0 0 0 1)
                   (1 0 0 1)
                   
                   (1 -1 -1 1)
                   (1 -1 0 1)
                   (0 -1 0 1)
                   (0 -1 0 1)
                   (0 -1 -1 1)
                   (1 -1 -1 1)))

(define (get-box x y z width height depth)
  (matrix-mult (switch-matrix (matrix-mult (get-translation-matrix (list x y z)) (matrix-mult (get-dialation-matrix (list width height depth)) 3d-identity-matrix))) unit-box))

(define (get-sphere-semicircles-helper semicircle rotation-matrix semicircles)
  (if (zero? semicircles)
      empty
      (let ([new-semicircle (matrix-mult rotation-matrix semicircle)])
        (cons new-semicircle (get-sphere-semicircles-helper new-semicircle rotation-matrix (sub1 semicircles))))))

(define (get-sphere-semicircles r semicircles)
    (get-sphere-semicircles-helper (get-semicircle-points r) (get-3d-rotation-matrix "x" (/ tau semicircles)) semicircles))

(define (get-sphere-slice-helper a b)
  (let ([next-a (cdr a)]
        [next-b (cdr b)])
    (if (or (empty? next-a) (empty? next-b))
        empty
        (append (if (or (empty? (cdr next-a)) (empty? (cdr next-b)))
                    empty
                    (list (car a) (car next-a) (car next-b)))
                (list (car a) (car next-b) (car b))
                (get-sphere-slice-helper next-a next-b)))))

(define (get-sphere-slice a b)
  (let ([next-a (cdr a)]
        [next-b (cdr b)])
    (append (list (car a) (car next-a) (car next-b))
            (get-sphere-slice-helper next-a next-b))))

(define (get-torus-slice-helper a b first-a first-b)
  (let ([next-a (cdr a)]
        [next-b (cdr b)])
    (if (or (empty? next-a) (empty? next-b))
        (list (car a) first-a first-b (car a) first-b (car b))
        (append (list (car a) (car next-a) (car next-b) (car a) (car next-b) (car b)) (get-torus-slice-helper next-a next-b first-a first-b)))))

(define (get-torus-slice a b)
  (get-torus-slice-helper a b (car a) (car b)))

(define (get-slices-helper slice-getter semicircles first-semicircle)
  (let ([next-semicircles (cdr semicircles)])
    (if (empty? next-semicircles)
        (slice-getter (car semicircles) first-semicircle)
        (append (slice-getter (car semicircles) (car next-semicircles)) (get-slices-helper slice-getter next-semicircles first-semicircle)))))

(define (get-slices slice-getter semicircles)
  (get-slices-helper slice-getter semicircles (car semicircles)))

(define (get-sphere x y z r)
  (matrix-mult (get-translation-matrix (list x y z)) (get-slices get-sphere-slice (get-sphere-semicircles r 20))))

(define (get-torus-circles-helper circle rotation-matrix circles)
  (if (zero? circles)
      empty
      (let ([new-circle (matrix-mult rotation-matrix circle)])
        (cons new-circle (get-torus-circles-helper new-circle rotation-matrix (sub1 circles))))))

(define (get-torus-circles r1 r2 circles)
    (get-torus-circles-helper (matrix-mult (get-translation-matrix (list r2 0 0)) (get-circle-points r1)) (get-3d-rotation-matrix "y" (- (/ tau circles))) circles))

(define (get-torus x y z r1 r2)
  (matrix-mult (get-translation-matrix (list x y z)) (get-slices get-torus-slice (get-torus-circles r1 r2 20))))