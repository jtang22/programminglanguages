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
  [numC (num : number)]
  [binop (sym : symbol) (a : ExprC) (b : ExprC)]
  [ifleq0 (test : ExprC) (then : ExprC) (else : ExprC)]
  [idC (x : symbol)]
  [appC (fn : symbol) (arg : ExprC)])

;representation of arithmetic functino for LOOI2
(define-type FundefC
  [fdC (name : symbol) (param : symbol) (body : ExprC)])

;lookup an operator for a binop expression
(define (operator-lookup [s : symbol])
  (cond
    [(equal? '+ s) +]
    [(equal? '* s) *]
    [(equal? '- s) -]
    [(equal? '/ s) /]
    [else (error 'parse "illegal operation")]))

;evaluation method for ExprC
(define (interp [a : ExprC]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (s) 5]
    [appC (f a) (error 'parse "appC eval")]
    [binop (s a b) ((operator-lookup s) (interp a) (interp b))]
    [ifleq0 (n t e)
            (cond
              [(= 0 (interp n)) (interp t)]
              [else (interp e)])]))

(test (interp (numC 5)) 5)
(test (interp (idC 'hello)) 5)
(test/exn (interp (appC 'function (numC 5))) "appC eval")
(test (interp (ifleq0 (numC 5) (numC 5) (binop '+ (numC 4) (numC 5)))) 9)
(test/exn (interp (binop '% (numC 5) (numC 6))) "illegal operation")

;Evaluates an s-expression and returns as an ExprC
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-match? '(+ ANY ANY) s)
          (binop '+ (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(* ANY ANY) s)
          (binop '* (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(- ANY ANY) s)
          (binop '- (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(/ ANY ANY) s)
          (binop '/ (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(ifleq0 ANY ANY ANY) s)
          (ifleq0 (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid input")]))]))
    ;[else (error 'parse "invalid list input")]))

(test (parse '(ifleq0 5 5 (+ 6 5))) (ifleq0 (numC 5) (numC 5) (binop '+ (numC 6) (numC 5))))
(test (parse '(+ 5 5)) (binop '+ (numC 5) (numC 5)))
(test (parse '(* 5 5)) (binop '* (numC 5) (numC 5)))
(test (parse '(- 5 5)) (binop '- (numC 5) (numC 5)))
(test/exn (parse '(+ 5 5 5)) "invalid input")

(define (top-eval [fun-sexps : s-expression])  : number
  (interp (parse fun-sexps)))

(test (top-eval '(ifleq0 5 5 (+ 6 5))) 11)
(test (top-eval '(ifleq0 0 5 (+ 6 5))) 5)
(test (top-eval '(+ 5 5)) 10)
(test (top-eval '(* 5 5)) 25)
(test (top-eval '(- 5 5)) 0)
(test (top-eval '(/ 25 5)) 5)
;Substitution
;(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
;  <subst-body>)


;(test (top-eval '(+ (* 5 6) (- 3))) 27)
;(test (top-eval '(- (+ 30 20) (* (+ 4 (- 2)) 2))) 46)
;(test (top-eval '(- (* 10 (+ 30 (+ 10 10))))) -500)

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

