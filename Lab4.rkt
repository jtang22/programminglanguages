#lang plai-typed
;Sample lambda functions
((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;Takes number a, returns function that takes number b, adds a + b
(define curried-add  
  (lambda (a)
    (lambda (b) (+ a b))))

(test ((curried-add 5) 3) 8)
(test ((curried-add 0) -5) -5)

