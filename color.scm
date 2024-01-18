#lang racket
(provide color color->list)

(struct color (r g b))

(define (color->list rangi)
  (list (color-r rangi) (color-g rangi) (color-b rangi)))