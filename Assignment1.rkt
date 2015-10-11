#lang plai-typed

;function calculates total profit for a movie theater
(define (total-profit [attendees : number]) : number
  (- (* 5 attendees) ( + (* .5 attendees) 20)))

(test (total-profit 0) -20)
(test (total-profit 1) -15.50)
(test (total-profit 5) 2.5)

(define PI 3.14159)

;function calculates the volume of a cylinder A = (2 pi r h) + 2 pi r^2)
(define (area-cylinder [radius : number] [height : number]) : number
  (+ (+ (area-of-circle radius) (area-of-circle radius)) (area-sides-cylinder radius height)))

;finds area of a circle
(define (area-of-circle [radius : number])
  (* PI (* radius radius)))

;finds area of sides of cylinder
(define (area-sides-cylinder [radius : number] [height : number])
  (* height (* 2 (* PI radius))))

(test (area-sides-cylinder 2 1) (* 4 PI))
(test (area-of-circle 2) (* 4 PI))
(test (area-cylinder 2 3) (* 20 PI))
(test (area-cylinder 3 4) (* 42 PI))

; Represent a magic trick
(define-type Trick
  [card-trick (decks : number) (volunteers : number)]
  [guillotine (realism : number) (has-blood? : boolean)])

;calculates the amount of time a magic trick will take
;deck - 1 min vol - doubles card trick per vol guill - 10 min blood - 20 min guill
(define (trick-minutes [trick : Trick]) : number
   (type-case Trick trick
     [card-trick(d v) (card-time d v)]
     [guillotine(r h) (guillotine-time r h)]))

;calculates the amount of time for a card trick
(define (card-time [decks : number] [volunteers : number]) : number
  (cond
    [(= volunteers 0) decks]
    [else (* 2 (card-time decks (- volunteers 1)))]))

;calculates the amount of time for guillotine
(define (guillotine-time [realism : number] [blood : boolean])
  (cond
    [blood 20]
    [else 10]))

(test (trick-minutes (card-trick 1 0)) 1)
(test (trick-minutes (card-trick 2 2)) 8)
(test (trick-minutes (guillotine 1 false)) 10)
(test (trick-minutes (guillotine 2 true)) 20)

;Definition for a polynomial
(define-type Polynomial
  [linear (A : number) (B : number)]
  [quadratic (A : number) (B : number) (C : number)])

;Evaluates result of polynomial for given value
;Ax + B, Ax^2 + Bx + C
(define (eval [poly : Polynomial] [value : number]) : number
  (type-case Polynomial poly
    [linear(a b) (+ (* a value) b)]
    [quadratic(a b c) (+ c (+ (* a (* value value)) (* b value)))]))

(test (eval (linear 1 1) 1) 2)
(test (eval (quadratic 1 2 3) 2) 11)

;Evaluates the derivative for a given polynomial
(define (derivative [poly : Polynomial]) : Polynomial
  (type-case Polynomial poly
    [linear(a b) (linear 0 a)]
    [quadratic(a b c) (linear (* 2 a) b)]))

(test (derivative (linear 1 1)) (linear 0 1))
(test (derivative (quadratic 1 2 3)) (linear 2 2))

;Data representation of binary tree
(define-type BTree
  [leaf (element : symbol)]
  [node (right-node : BTree) (left-node : BTree)])

(define leaf1 (leaf 'horse))
(define leaf2 (leaf 'shoe))
(define leaf3 (leaf 'three))
(define leaf4 (leaf 'four))
(define leaf5 (leaf 'five))
(define leaf6 (leaf 'six))
(define node1 (node leaf3 leaf4))
(define node2 (node leaf5 leaf6))
(define node3 (node leaf4 leaf3))
(define node4 (node leaf6 leaf5))
(define root1 (node leaf1 leaf2))
(define root2 (node leaf2 leaf1))

(define root3 (node node1 node2))
(define root4(node node4 node3))

;Mirrors a given binary tree
(define (mirror [root : BTree]) : BTree
  (type-case BTree root
    [leaf(e) root]
    [node(l r) (node (mirror r) (mirror l))]))

(test (mirror root1) root2)
(test (mirror root3) root4)

;Traverses a binary tree in-order
(define (in-order [root : BTree]) : (listof symbol)
  (type-case BTree root
    [leaf(e) (cons e empty)]
    [node(l r) (my-append (in-order l) (in-order r))]))

;Consumes two lists, appends the second one to the first
(define (my-append [list1 : (listof symbol)] [list2 : (listof symbol)]) : (listof symbol)
  (cond
    [(empty? list1) list2]
    [else (cons (first list1) (my-append (rest list1) list2))]))

(test (my-append (list 'horse 'shoe) (list 'three 'four)) (list 'horse 'shoe 'three 'four))
(test (in-order root1) (list 'horse 'shoe))
(test (in-order root3) (list 'three 'four 'five 'six))
