#lang racket
(provide get-rotation-matrix get-identity-matrix get-translation-matrix get-dialation-matrix)

(define (get-rotation-vector dim 1st 2nd sine cosine n)
  (build-list (add1 dim) (cond
                           [(= n 1st) (lambda (i)
                                        (cond
                                          [(= i 1st) cosine]
                                          [(= i 2nd) (- sine)]
                                          [else 0]))]
                           [(= n 2nd) (lambda (i)
                                        (cond
                                          [(= i 1st) sine]
                                          [(= i 2nd) cosine]
                                          [else 0]))]
                           [else (lambda (i)
                                   (if (= i n)
                                       1
                                       0))])))

(define (get-rotation-matrix dim a b angle)
  (let ([sine (sin angle)]
        [cosine (cos angle)]
        [1st (min a b)]
        [2nd (max a b)])
    (build-list (add1 dim) (lambda (i)
                             (get-rotation-vector dim 1st 2nd sine cosine i)))))

(define (get-identity-vector dim n)
  (build-list (add1 dim) (lambda (i)
                           (if (= i n)
                               1
                               0))))

(define (get-identity-matrix dim)
  (build-list (add1 dim) (lambda (i)
                           (get-identity-vector dim i))))

(define (get-matrix-helper vector-maker dim values n)
  (if (empty? values)
      (cons (get-identity-vector dim n) empty)
      (cons (vector-maker dim (car values) n) (get-matrix-helper vector-maker dim (cdr values) (add1 n)))))

(define (get-matrix vector-maker values)
  (get-matrix-helper vector-maker (length values) values 0))

(define (get-translation-vector dim value n)
  (build-list (add1 dim) (lambda (i)
                           (cond
                             [(= i n) 1]
                             [(= i dim) value]
                             [else 0]))))

(define (get-translation-matrix values)
  (get-matrix get-translation-vector values))

(define (get-dialation-vector dim value n)
  (build-list (add1 dim) (lambda (i)
                           (if (= i n)
                               value
                               0))))

(define (get-dialation-matrix values)
  (get-matrix get-dialation-vector values))