#lang racket
(provide get-edge get-circle-points get-circle get-semicircle-points get-hermite get-bezier)

(require racket/math "matrix.scm" "get-matrix.scm" "get-threed-matrix.scm" "connect.scm")

(define tau (* pi 2))

(define (get-arc-points-helper point rotation-matrix points)
  (if (zero? points)
      empty
      (let ([new-point (matrix-vector-mult rotation-matrix point)])
        (cons new-point (get-arc-points-helper new-point rotation-matrix (sub1 points))))))

(define (get-point x y z)
  (list x y z 1))

(define (get-edge start-x start-y start-z end-x end-y end-z)
  (list (get-point start-x start-y start-z) (get-point end-x end-y end-z)))

(define (get-arc-points r angle points)
  (get-arc-points-helper (get-point r 0 0) (get-3d-rotation-matrix "z" (/ angle points)) points))

(define (get-circle-points r)
  (get-arc-points r tau 20))

(define (get-circle x y z r)
  (connect-closed (matrix-mult (get-translation-matrix (list x y z)) (get-circle-points r))))

(define (get-semicircle-points r)
  (cons (get-point r 0 0) (get-arc-points r pi 10)))

(define (get-polynomial-helper coefs)
  (if (empty? coefs)
      (lambda (n)
        0)
      (let ([coef (car coefs)]
            [next-polynomial (get-polynomial-helper (cdr coefs))])
        (lambda (n)
          (+ coef (* n (next-polynomial n)))))))

(define (get-polynomial coefs)
  (get-polynomial-helper (reverse coefs)))

(define (get-cubic-points ax bx cx dx ay by cy dy points)
  (let ([polyx (get-polynomial (list ax bx cx dx))]
        [polyy (get-polynomial (list ay by cy dy))])
    (let ([get-cubic-point (lambda (p)
                             (let ([t (/ p points)])
                               (get-point (polyx t) (polyy t) 0)))])
      (build-list points get-cubic-point))))

(define (get-cubic ax bx cx dx ay by cy dy)
  (connect-open (get-cubic-points ax bx cx dx ay by cy dy 100)))

(define hermite-matrix '((2 -2 1 1)
                         (-3 3 -2 -1)
                         (0 0 1 0)
                         (1 0 0 0)))

(define (get-hermite x0 y0 x1 y1 rx0 ry0 rx1 ry1)
  (apply get-cubic (append (matrix-vector-mult hermite-matrix (list x0 x1 rx0 rx1)) (matrix-vector-mult hermite-matrix (list y0 y1 ry0 ry1)))))

(define bezier-matrix '((-1 3 -3 1)
                        (3 -6 3 0)
                        (-3 3 0 0)
                        (1 0 0 0)))

(define (get-bezier x0 y0 x1 y1 x2 y2 x3 y3)
  (apply get-cubic (append (matrix-vector-mult bezier-matrix (list x0 x1 x2 x3)) (matrix-vector-mult bezier-matrix (list y0 y1 y2 y3)))))
