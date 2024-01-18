#lang racket
(provide get-light)

(require "vector.scm" "color.scm")

(define (cap-at-0 n)
  (if (negative? n)
      0
      n))

(define (cap-at-255 n)
  (if (> n 255)
      255
      n))

(define (get-single-light sa sp ska skd sks l n x)
  (exact-round (cap-at-255 (+ (cap-at-0 (* sa ska))
                              (cap-at-0 (* sp skd (dot-mult n l)))
                              (cap-at-0 (* sp sks (expt (* (third l) (sub1 (* 2 (sqr (third n))))) x)))))))

(define (get-light a p ka kd ks l n x)
  (let ([get-some-single-light (lambda (sa sp ska skd sks)
                                 (get-single-light sa sp ska skd sks l n x))])
    (apply color (map get-some-single-light (color->list a) (color->list p) (color->list ka) (color->list kd) (color->list ks)))))