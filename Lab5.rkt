#lang plai-typed

;; solution
;(define p1 (lambda (v) (lambda (f) v)))
 
;; test case:
;(test ((p1 8) (lambda (x) 1234)) 8)

(define (testZ [n : number])
  (+ n 5))

(test (testZ 5) 10)

;Accept a function and argument, applies function to argument
(define one (lambda (f)
              (lambda (a) (f a))))

(test ((one (lambda(x) (+ 5 x))) 8) 13)

;Accepts a function and an argument and applies the function to the result of applying the function to the argument
(define two (lambda(f) (lambda (a) (f (f a)))))

(test ((two (lambda (x)
                 (+ x 5))) 5) 15)

;Accepts a function and an argument and returns the argument
(define zero (lambda (f) (lambda (a) a)))

(test ((zero testZ) 5) 5)

;Accepts a number-like function and does function "one more time"
(define add1 (lambda (num-func func)
               (lambda(x)
                 ((one func) ((num-func func) x)))))

(test ((add1 one (lambda(x) (+ x 3))) 4) 10)

;Accepts a number-like function and applies it the total number of times
(define add (lambda (num-func1 num-func2 func)
              (lambda(x)
                ((num-func2 func) ((num-func1 func) x)))))

(test ((add one two (lambda(x) (+ x 5))) 5) 20)
(test ((add two two (lambda(x) (* x 2))) 2) 32)

;Accepts two arguments and returns the first one
(define tru (lambda (one two)
              one))

(test (tru 5 6) 5)

;Accepts two arguments and returns the second one
(define fals (lambda (one two)
               two))

(test (fals 5 6) 6)

;Accepts three arguments
(define if (lambda (bool x y)
             (bool x y)))

(test (if tru 3 5) 3)
(test (if fals 3 5) 5)



