#lang plai-typed
; function for converting dollars to euro
(define (dollar->euro [dollar : number]) : number
  (* dollar .90))
(test (dollar->euro 5) 4.5)

;function sums money from a number of pennies, nickels, dimes, and quarters
(define (sum-coins [pennies : number] [nickels : number] [dimes : number] [quarters : number]) : number
  (+ (* quarters .25) (+ (* dimes .1) (+ (* pennies .01) (* nickels .05)))))

(test (sum-coins 1 1 1 1) .41)

;function that determines the interest rate of a bank account
(define (interest [amount : number]) : number
  (cond
    [(<= amount 1000) (* amount .04)]
    [(<= amount 5000) (* amount .045)]
    [(> amount 5000) (* amount .05)]))

(test (interest 400) 16)
(test (interest 4000) 180)
(test (interest 5500) 275)

;function determines how many solutions a quadratic equation has
(define (how-many [a : number] [b : number] [c : number]) : number
  (cond
    [(< (* 4 (* a c)) (* b b)) 2]
    [(= (* 4 (* a c)) (* b b)) 1]
    [(> (* 4 (* a c)) (* b b)) 0]))

(test (how-many 1 0 -1) 2)
(test (how-many 2 4 2) 1)
(test (how-many 5 1 2) 0)

;function determines how many solutions a quadratic equation has
(define (what-kind [a : number] [b : number] [c : number]) : symbol
  (cond
    [(= a 0) 'degenerate]
    [(< (* 4 (* a c)) (* b b)) 'two]
    [(= (* 4 (* a c)) (* b b)) 'one]
    [(> (* 4 (* a c)) (* b b)) 'none]))

(test (what-kind 1 0 -1) 'two)
(test (what-kind 2 4 2) 'one)
(test (what-kind 5 1 2) 'none)
(test (what-kind 0 1 1) 'degenerate)

;furniture type
(define-type Furniture
  [desk (width : number) (height : number) (depth : number)]
  [bookshelf (depth : number) (num-shelves : number) (shelf-width : number)])
(define desk1(desk 100 100 100))
(define bshelf1(bookshelf 100 5 10))

;function figure out the footprint of a piece of furniture
(define (furniture-footprint [furniture : Furniture]) : number
  (type-case Furniture furniture
    [desk(w h d) (* w d)]
    [bookshelf(d n w) (* d w)]))

(test (furniture-footprint desk1) 10000)
(test (furniture-footprint bshelf1) 1000)
  