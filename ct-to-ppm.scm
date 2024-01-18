#lang racket
(provide ct->ppm)

(require "table.scm" "color.scm" "pair.scm")

(define (ct->number-list ct)
  (cons 255 (apply append (map color->list (map pair-val (table->list ct))))))

(define (ct->string-list ct)
  (cons "P3" (map number->string (append (get-width-height ct) (ct->number-list ct)))))

(define (ct->ppm ct)
  (string-append (string-join (ct->string-list ct)) "\n"))