#lang plai-typed
(require plai-typed/s-exp-match)

;representation of arithmetic expression
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

;evaluation method for arithc
(define (eval [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (eval l) (eval r))]
    [multC (l r) (* (eval l) (eval r))]))

(test (eval (numC 5)) 5)
(test (eval (plusC (numC 1) (numC 2))) 3)
(test (eval (multC (numC 2) (numC 3))) 6)

;Counts the numbers in an ArithC expression
(define (num-nums [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) 1]
    [plusC (l r) (+ (num-nums l) (num-nums r))]
    [multC (l r) (+ (num-nums l) (num-nums r))]))

(test (num-nums (numC 5)) 1)
(test (num-nums (plusC (numC 1) (numC 2))) 2)
(test (num-nums (multC (numC 2) (plusC (numC 1) (numC 2)))) 3)

;Representation of arithmetic expression including minus
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (l : ArithS)])

;Parse for ArthS language
(define (parse [s : s-expression]) : ArithC
  (desugar (parser s)))

;Evaluates an s-expression and returns as an ArithS
;Rename to parser
(define (parser [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-match? '(+ ANY ANY) s)
          (plusS (parser (second sl)) (parser (third sl)))]
         [(s-exp-match? '(* ANY ANY) s)
          (multS (parser (second sl)) (parser (third sl)))]
         [(s-exp-match? '(- ANY ANY) s)
          (bminusS (parser (second sl)) (parser (third sl)))]
         [(s-exp-match? '(- ANY) s)
          (uminusS (parser (second sl)))]
         [else (error 'parse "invalid input")]))]))
    ;[else (error 'parse "invalid list input")]))

;Desugarer for Arith parser
(define (desugar [as : ArithS]) : ArithC
 (type-case ArithS as
   [numS (n) (numC n)]
   [plusS (l r) (plusC (desugar l)
                       (desugar r))]
   [multS (l r) (multC (desugar l)
                       (desugar r))]
   [bminusS (l r) (plusC (desugar l)
                         (desugar (uminusS r)))]
   [uminusS (l) (multC (numC -1) (desugar l))]))

(test (parse '(+ 3 2)) (plusC (numC 3) (numC 2)))
(test (parse '(* 3 2)) (multC (numC 3) (numC 2)))
(test (parse '(- 3 2)) (plusC (numC 3) (multC (numC -1) (numC 2))))
(test (parse '(+ (* 1 2) 3)) (plusC (multC (numC 1) (numC 2)) (numC 3)))
(test (parse '(- 3 2)) (plusC (numC 3) (multC (numC -1) (numC 2))))
(test (parse '(- 3)) (multC (numC -1) (numC 3)))
(test (parse '(+ (* 5 6) (- 3))) (plusC (multC (numC 5) (numC 6)) (multC (numC -1) (numC 3))))
(test (parse '(- (* 3 (+ 5 10)))) (multC (numC -1) (multC (numC 3) (plusC (numC 5) (numC 10)))))
(test/exn (parse '(+ 5 5 5)) "invalid input")

;Calls parser then eval function for an ArithS
(define (parse-eval [s : s-expression]) : number
  (eval (parse s)))

(test (parse-eval '(+ (* 5 6) (- 3))) 27)
(test (parse-eval '(- (+ 30 20) (* (+ 4 (- 2)) 2))) 46)
(test (parse-eval '(- (* 10 (+ 30 (+ 10 10))))) -500)

(define (multThree [n : number]) : number
  (eval (multC (numC 3) (numC n))))

(define (addTwo [n : number]) : number
  (eval (plusC (numC 2) (numC n))))


;Consumes a list and a function, applies function twice to each element in the list
(define (doublemap [f : ('a -> 'a)] [numList : (listof number)]) : (listof number)
  (cond
    [(empty? numList) numList]
    [else (cons (f(f (first numList))) (doublemap f (rest numList)))]))

(test (doublemap multThree (list 1 2 3 4 5)) (list 9 18 27 36 45))
(test (doublemap addTwo (list 1 2 3 4 5)) (list 5 6 7 8 9))

;Consumes two lists and zips them together
(define (zip [list1 : (listof number)] [list2 : (listof number)]) : (listof (listof number))
  (cond
    [(empty? list1) empty]
    [else (cons (list (first list1) (first list2)) (zip (rest list1) (rest list2)))]))

(test (zip (list 1 2 3 4 5) (list 11 12 13 14 15)) (list (list 1 11) (list 2 12) (list 3 13) (list 4 14) (list 5 15)))
(test (zip empty empty) empty)