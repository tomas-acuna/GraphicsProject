#lang racket
(provide try-pair get-starting-pair pair-val)

(struct pair (val z))

(define (greater? a b)
  (if a
      (if b
          (> a b)
          #t)
      #f))

(define (try-pair val z other-pair)
  (if (greater? z (pair-z other-pair))
      (pair val z)
      other-pair))

(define (get-starting-pair val)
  (pair val #f))