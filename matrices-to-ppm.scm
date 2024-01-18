#lang racket
(provide matrices->ppm)

(require "line.scm" "triangle.scm" "ct-to-ppm.scm" "table.scm" "color.scm" "pair.scm" "vector.scm" "light.scm")

(define (point-line table start end value)
  (line table (first start) (second start) (third start) (first end) (second end) (third end) value))

(define (point-triangle table a b c value)
  (triangle table (first a) (second a) (third a) (first b) (second b) (third b) (first c) (second c) (third c) value))

(define (draw-edge-matrix table edge-matrix value)
  (if (empty? edge-matrix)
      table
      (let ([uno (first edge-matrix)]
            [dos (second edge-matrix)]
            [resto (cddr edge-matrix)])
        (draw-edge-matrix (point-line table uno dos value) resto value))))

(define (get-surface-normal point0 point1 point2)
  (cross-mult (map - point1 point0) (map - point2 point0)))

(define light-source (normalize '(-1 0 1)))

(define (draw-tri-matrix table tri-matrix)
  (if (empty? tri-matrix)
      table
      (let ([uno (first tri-matrix)]
            [dos (second tri-matrix)]
            [tres (third tri-matrix)]
            [resto (cdddr tri-matrix)])
        (draw-tri-matrix
         (let ([surface-normal (get-surface-normal uno dos tres)])
           (if (positive? (third surface-normal))
               ;(get-light (color 0.5 0.5 0.5) (color 0.5 0.5 0) (color 100 200 100) (color 100 200 100) (color 100 200 100) light-source (normalize surface-normal) 2)
               (point-triangle table uno dos tres (get-light (color 0.5 0.5 0.5) (color 0.5 0.5 0) (color 100 200 100) (color 100 200 100) (color 100 200 100) light-source (normalize surface-normal) 2))
               table))
         resto))))

(define (round-vector vector)
  (map exact-round vector))

(define (round-matrix matrix)
  (map round-vector matrix))

(define (matrices->ppm edge-matrix tri-matrix width height background-color line-color)
  (ct->ppm (draw-edge-matrix (draw-tri-matrix (get-table width height (get-starting-pair background-color)) (round-matrix tri-matrix)) (round-matrix edge-matrix) line-color)))
