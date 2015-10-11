#lang plai-typed

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
