#lang racket
(provide file->syst)

(require "get-matrix.scm" "get-threed-matrix.scm" "matrix.scm" "color.scm" "file-to-insts.scm" "matrices-to-ppm.scm" "color.scm" "curve.scm" "threed.scm")

(struct syst (trans-matrices edge-matrix tri-matrix))

(define (get-rotation axis dangle)
  (get-3d-rotation-matrix axis (degrees->radians dangle)))

(define (apply-inst inst sistema)
  (let ([command (car inst)]
        [args (cdr inst)]
        [trans-matrices (syst-trans-matrices sistema)]
        [edge-matrix (syst-edge-matrix sistema)]
        [tri-matrix (syst-tri-matrix sistema)])
    (let ([trans-matrix (car trans-matrices)])
      (let ([translate (lambda (matrix) (matrix-mult (switch-matrix trans-matrix) matrix))]
            [get-new-trans-matrices (lambda (matrix) (cons (matrix-mult (switch-matrix trans-matrix) (switch-matrix matrix)) (cdr trans-matrices)))])
        (cond [(equal? command "line")
               (syst trans-matrices (append (translate (apply get-edge args)) edge-matrix) tri-matrix)]
              [(equal? command "circle")
               (syst trans-matrices (append (translate (apply get-circle args)) edge-matrix) tri-matrix)]
              [(equal? command "hermite")
               (syst trans-matrices (append (translate (apply get-hermite args)) edge-matrix) tri-matrix)]
              [(equal? command "bezier")
               (syst trans-matrices (append (translate (apply get-bezier args)) edge-matrix) tri-matrix)]
              [(equal? command "box")
               (syst trans-matrices edge-matrix (append (translate (apply get-box args)) tri-matrix))]
              [(equal? command "sphere")
               (syst trans-matrices edge-matrix (append (translate (apply get-sphere args)) tri-matrix))]
              [(equal? command "torus")
               (syst trans-matrices edge-matrix (append (translate (apply get-torus args)) tri-matrix))]
              [(equal? command "scale")
               (syst (get-new-trans-matrices (get-dialation-matrix args)) edge-matrix tri-matrix)]
              [(equal? command "move")
               (syst (get-new-trans-matrices (get-translation-matrix args)) edge-matrix tri-matrix)]
              [(equal? command "rotate")
               (syst (get-new-trans-matrices (apply get-rotation args)) edge-matrix tri-matrix)]
              [(equal? command "push")
               (syst (cons trans-matrix trans-matrices) edge-matrix tri-matrix)]
              [(equal? command "pop")
               (syst (cdr trans-matrices) edge-matrix tri-matrix)]
              [(equal? command "save")
               (let ([ppm (matrices->ppm edge-matrix tri-matrix 500 500 (color 255 255 255) (color 255 0 255))]
                     [port (open-output-file (car args) #:exists 'replace)])
                 (begin
                   (display ppm port)
                   (close-output-port port)
                   sistema))]
              [(equal? command "display")
               (let ([ppm (matrices->ppm edge-matrix tri-matrix 500 500 (color 255 255 255) (color 255 0 255))]
                     [port (open-output-file "display.ppm")])
                 (begin
                   (display ppm port)
                   (close-output-port port)
                   (system "display display.ppm")
                   (delete-file "display.ppm")
                   sistema))]
              [else
               sistema])))))

(define (file->syst file)
  (foldl apply-inst (syst (list 3d-identity-matrix) empty empty) (file->insts file)))