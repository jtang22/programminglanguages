#lang plai-typed
;Sample lambda functions
((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;Takes number a, returns function that takes number b, adds a + b
;(number -> (number -> number))
(define curried-add  
  (lambda (a)
    (lambda (b) (+ a b))))

(test ((curried-add 5) 3) 8)
(test ((curried-add 0) -5) -5)

;Takes a function of two arguments, produces a function that takes one argument,
;Produces a function that takes one argument and produces the result of calling input function on two given arguments
;(a b -> c) -> (a -> (b -> c))
(define curry2
  (lambda (a b)
    (lambda (c) )))
