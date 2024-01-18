#lang racket
(provide file->insts)

(define (no-arguments? command)
  (member command '("display" "pop" "push")))

(define (lines->insts lines)
  (if (empty? lines)
      empty
      (let ([command (car lines)]
            [resto (cdr lines)])
        (if (no-arguments? command)
            (cons (cons command empty) (lines->insts resto))
            (cons (cons command (map s->n (string-split (car resto)))) (lines->insts (cdr resto)))))))

(define (s->n s)
  (let ([n (string->number s)])
    (if n n s)))

(define (comment? string)
  (equal? (car (string->list string)) #\#))

(define (file->insts file)
  (lines->insts (filter-not comment? (map string-trim (string-split (string-trim (file->string file)) "\n" #:repeat? #t)))))