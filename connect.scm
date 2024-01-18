#lang racket
(provide double-list connect-open connect-closed connect-two)

(define (double-list listo)
  (if (empty? listo)
      empty
      (let ([item (car listo)])
        (cons item (cons item (double-list (cdr listo)))))))

(define (remove-last listo)
  (let ([next (cdr listo)])
    (if (empty? next)
        empty
        (cons (car listo) (remove-last next)))))

(define (add-to-end listo item)
  (if (empty? listo)
      (cons item empty)
      (cons (car listo) (add-to-end (cdr listo) item))))

(define (connect-open listo)
  (remove-last (cdr (double-list listo))))

(define (connect-closed listo)
  (add-to-end (cdr (double-list listo)) (car listo)))

(define (connect-two a b)
  (if (or (empty? a) (empty? b))
      empty
      (cons (car a) (cons (car b) (connect-two (cdr a) (cdr b))))))