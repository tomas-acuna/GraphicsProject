#lang racket
(provide matrix-vector-mult matrix-mult matrix-a->string matrix-b->string switch-matrix)

(require "vector.scm")

(define (matrix-vector-mult matrix vector)
  (let ([get-value (lambda (v)
                     (dot-mult vector v))])
    (map get-value matrix)))

(define (matrix-mult a b)
  (let ([get-vector (lambda (v)
                      (matrix-vector-mult a v))])
    (map get-vector b)))

(define (vector->string vector)
  (string-join (map number->string vector)))

(define (lines->string lines)
  (string-join lines "\n"))

(define (matrix-a->string matrix)
  (lines->string (map vector->string matrix)))

(define (switch-matrix matrix)
  (if (empty? (car matrix))
      empty
      (cons (map car matrix) (switch-matrix (map cdr matrix)))))

(define (matrix-b->string matrix)
  (matrix-a->string (switch-matrix matrix)))