#lang racket
(provide get-3d-rotation-matrix 3d-identity-matrix)

(require "get-matrix.scm")

(define (get-3d-rotation-matrix axis angle)
  (cond [(equal? axis "x") (get-rotation-matrix 3 1 2 angle)]
        [(equal? axis "y") (get-rotation-matrix 3 0 2 (- angle))]
        [(equal? axis "z") (get-rotation-matrix 3 0 1 angle)]))

(define 3d-identity-matrix (get-identity-matrix 3))