#lang plai-typed
(require plai-typed/s-exp-match)

;  LOOI2	 	=	 	num
; 	 	|	 	{+ LOOI2 LOOI2}
; 	 	|	 	{- LOOI2 LOOI2}
; 	 	|	 	{* LOOI2 LOOI2}
; 	 	|	 	{/ LOOI2 LOOI2}
; 	 	|	 	{id :D LOOI2 ...}
; 	 	|	 	{ifleq0 LOOI2 LOOI2 LOOI2}
; 	 	|	 	id
;  DEFN	 	=	 	{func id id ... LOOI2}


;representation of arithmetic expression
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifleq0 (num : ExprC)(fn : symbol)])

;evaluation method for ExprC
(define (interp [a : ExprC]) : number
  (type-case ExprC a
    [numC (n) n]
    ;[idC (s : symbol) ]
    ;[appC (fun : symbol) (arg : ExprC)]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifleq0 (n e) (error 'parse "ifleq branch")]))

(test (interp (numC 5)) 5)
(test (interp (plusC (numC 1) (numC 2))) 3)
(test (interp (multC (numC 2) (numC 3))) 6)
(test/exn (interp (ifleq0 (numC 5) 'hello)) "ifleq branch")

;Counts the numbers in an ExprC expression
;(define (num-nums [a : ExprC]) : number
;  (type-case ExprC a
;    [numC (n) 1]
;    [plusC (l r) (+ (num-nums l) (num-nums r))]
;    [multC (l r) (+ (num-nums l) (num-nums r))]))

;(test (num-nums (numC 5)) 1)
;(test (num-nums (plusC (numC 1) (numC 2))) 2)
;(test (num-nums (multC (numC 2) (plusC (numC 1) (numC 2)))) 3)

;Representation of arithmetic expression including minus
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [uminusS (l : ExprS)])

;Parse for ArthS language
(define (parse-prog [s : s-expression]) : ExprC
  (desugar (parser s)))

;Evaluates an s-expression and returns as an ExprS
;Rename to parser
(define (parser [s : s-expression]) : ExprS
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
         [(s-exp-match? '(SYMBOL) '(ifleq))
          (error 'parse "ifleq parse branch")]
         [else (error 'parse "invalid input")]))]))
    ;[else (error 'parse "invalid list input")]))

;Desugarer for Arith parser
(define (desugar [as : ExprS]) : ExprC
 (type-case ExprS as
   [numS (n) (numC n)]
   [plusS (l r) (plusC (desugar l)
                       (desugar r))]
   [multS (l r) (multC (desugar l)
                       (desugar r))]
   [bminusS (l r) (plusC (desugar l)
                         (desugar (uminusS r)))]
   [uminusS (l) (multC (numC -1) (desugar l))]))

(test (parse-prog '(+ 3 2)) (plusC (numC 3) (numC 2)))
(test (parse-prog '(* 3 2)) (multC (numC 3) (numC 2)))
(test (parse-prog '(- 3 2)) (plusC (numC 3) (multC (numC -1) (numC 2))))
(test (parse-prog '(+ (* 1 2) 3)) (plusC (multC (numC 1) (numC 2)) (numC 3)))
(test (parse-prog '(- 3 2)) (plusC (numC 3) (multC (numC -1) (numC 2))))
(test (parse-prog '(- 3)) (multC (numC -1) (numC 3)))
(test (parse-prog '(+ (* 5 6) (- 3))) (plusC (multC (numC 5) (numC 6)) (multC (numC -1) (numC 3))))
(test (parse-prog '(- (* 3 (+ 5 10)))) (multC (numC -1) (multC (numC 3) (plusC (numC 5) (numC 10)))))
(test/exn (parse-prog 'ifleq0))
(test/exn (parse-prog '(+ 5 5 5)) "invalid input")

(define (top-eval [fun-sexps : s-expression])  : number
  (interp (parse-prog fun-sexps)))


(test (top-eval '(+ (* 5 6) (- 3))) 27)
(test (top-eval '(- (+ 30 20) (* (+ 4 (- 2)) 2))) 46)
(test (top-eval '(- (* 10 (+ 30 (+ 10 10))))) -500)

;(define (parse [s : s-expression]) : ExprC
;)

;(define (parse-fundef [s : s-expression]) : FundefC
;)

;(define (parse-prog [s : s-expression]) : listof FundefC
;)

;(define (interp funs : (listof FundefC) : number
;)

;(define (top-eval [fun-exps : listof s-expression]) : number
;)

