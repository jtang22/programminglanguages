#lang plai-typed
;Sample lambda functions
((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;Takes number a, returns function that takes number b, adds a + b
;(number -> (number -> number))
(define (curried-add [a : number]) : (number -> number)
  (lambda (b) (+ a b)))

(test ((curried-add 5) 3) 8)
(test ((curried-add 0) -5) -5)

;Takes a function of two arguments, produces a function that takes one argument,
;Produces a function that takes one argument and produces the result of calling the input function on two given arguments
;(a b -> c) -> (a -> (b -> c))
(define (curry2 func)
  (lambda (a)
    (lambda (b) (func a b))))

(test (((curry2 (lambda (a b) (+ a b))) 2) 4) 6)
(test (((curry2 (lambda (a b) (* (- a b) b))) 5) 3) 6)

;Takes function of three arguments, produces a function that takes one argument,
;Produces a function that takes one argument and produces another function that produces
;the result of calling the input function on all three arguments
(define (curry3 func)
  (lambda (a)
    (lambda (b)
      (lambda (c) (func a b c)))))

(test ((((curry3 (lambda (a b c) (+ (+ a b) c))) 1) 2) 3) 6)
(test ((((curry3 (lambda (a b c) (* a (+ a (- c b))))) 2) 5) 6) 6)