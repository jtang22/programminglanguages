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
  [appC (fn : symbol) (arg : (listof ExprC))])

;representation of arithmetic function for LOOI2
(define-type FundefC
  [fdC (name : symbol) (param : (listof symbol)) (body : ExprC)])

;lookup an operator for a binop expression
(define (operator-lookup [s : symbol])
  (cond
    [(equal? '+ s) +]
    [(equal? '* s) *]
    [(equal? '- s) -]
    [(equal? '/ s) /]
    [else (error 'parse "illegal operation")]))

;Evaluates an s-expression and returns as an ExprC
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-match? '(NUMBER) s)
          (parse(first sl))]
         [(s-exp-match? '(SYMBOL) s)
          (parse (first sl))]
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
         [(s-exp-match? '(SYMBOL ANY ...) s)
          (appC (s-exp->symbol (first sl)) (map parse (rest (rest sl))))]
         [else (error 'parse "invalid input")]))]))

(test (parse '(ifleq0 5 5 (+ 6 5))) (ifleq0 (numC 5) (numC 5) (binop '+ (numC 6) (numC 5))))
(test (parse '(+ 5 5)) (binop '+ (numC 5) (numC 5)))
(test (parse '(* 5 5)) (binop '* (numC 5) (numC 5)))
(test (parse '(- 5 5)) (binop '- (numC 5) (numC 5)))
(test (parse '(/ 5 5)) (binop '/ (numC 5) (numC 5)))
(test/exn (parse '(5 5 5 5)) "invalid input")
(test (parse '(f :D 5)) (appC 'f (list (numC 5))))
(test (parse '(f)) (idC 'f))
(test (parse '(5)) (numC 5))

;parser for function definitions
(define (parse-fundef [fundef : s-expression]) : FundefC
  (cond
    [(s-exp-list? fundef)
     (let ([sl (s-exp->list fundef)])
       (cond
         [(s-exp-match? '(func ANY ... ANY) fundef)
          (fdC (s-exp->symbol (second sl))
               (map s-exp->symbol (reverse (rest (reverse (rest (rest sl))))))
               (parse (first (reverse sl))))]
         [else (error 'parse "invalid function input")]))]))

(test (parse-fundef '(func hello x (+ x x))) (fdC 'hello (list 'x) (binop '+ (idC 'x) (idC 'x))))
(test (parse-fundef '(func hello x y (+ x x))) (fdC 'hello (list 'x 'y) (binop '+ (idC 'x) (idC 'x))))
(test/exn (parse-fundef '(func)) "invalid function input")

;Check for a dup in a list
(define (check-dups [s : symbol] [params :(listof symbol)]) : (listof symbol)
  (cond
    [(empty? params) (list s)]
    [(equal? s (first params)) (cons s (check-dups s (rest params)))]
    [(equal? s '+) (error 'parse "bad syntax")]
    [(equal? s '-) (error 'parse "bad syntax")]
    [(equal? s '*) (error 'parse "bad syntax")]
    [(equal? s '/) (error 'parse "bad syntax")]
    [(equal? s 'ifleq0) (error 'parse "bad syntax")]
    [else (check-dups s (rest params))]))

;Checks params of the functions
(define (check-params [params : (listof symbol)]) : (listof symbol)
  (cond
    [(empty? params) params]
    [(= 1 (length params)) params]
    [(= 1 (length (check-dups (first params) (rest params))))
     (cons (first params) (check-params (rest params)))]
    [else (error 'parse "bad syntax")]))
    ;[else (cons (check-dups (first params) (rest params)) (check-params (rest params)))]))

;Checks variables in function
(define (check-body [body : ExprC]) : ExprC
  (type-case ExprC body
    [numC (n) body]
    [idC (s) (cond
               [(equal? s '+) (error 'parse "bad syntax")]
               [(equal? s '-) (error 'parse "bad syntax")]
               [(equal? s '*) (error 'parse "bad syntax")]
               [(equal? s '/) (error 'parse "bad syntax")]
               [(equal? s 'ifleq0) (error 'parse "bad syntax")])]
    [appC (s a) (appC s (map check-body a))]
    [binop (s a b) (binop s (check-body a) (check-body b))]
    [ifleq0 (n t e) (ifleq0 (check-body n) (check-body t) (check-body e))]))

;Checks to make sure all functions are good syntax
(define (check-funcs [funcs : (listof FundefC)]) : (listof FundefC)
  (cond
    [(empty? funcs) funcs]
    [else (type-case FundefC (first funcs)
            [fdC (n p b) (cons (fdC n (check-params p) (check-body b))
                               (check-funcs (rest funcs)))])]))

(test (check-funcs (list (fdC 'h (list) (numC 5)))) (list (fdC 'h (list) (numC 5))))
(test (check-funcs (list (fdC 'h (list 'x 'y) (numC 5)))) (list (fdC 'h (list 'x 'y) (numC 5))))
(test (check-funcs (list (fdC 'h (list 'x 'y) (appC 'f (list (appC 'h (list (binop 'x (numC 5) (numC 6)))))))))
      (list (fdC 'h (list 'x 'y) (appC 'f (list (appC 'h (list (binop 'x (numC 5) (numC 6)))))))))
(test (check-funcs (list (fdC 'h (list 'x 'y) (ifleq0 (numC 5) (numC 6) (numC 5)))))
      (list (fdC 'h (list 'x 'y) (ifleq0 (numC 5) (numC 6) (numC 5)))))
(test (check-funcs (list (fdC 'h (list 'x) (numC 5)) (fdC 'f (list 'y) (numC 6))))
      (list (fdC 'h (list 'x) (numC 5)) (fdC 'f (list 'y) (numC 6))))
(test/exn (check-funcs (list (fdC 'h (list 'x 'y) (idC '+)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list 'x 'y) (idC '-)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list 'x 'y) (idC '*)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list 'x 'y) (idC '/)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list 'x 'y) (idC 'ifleq0)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list '+ 'y) (idC 'x)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list '/ 'y) (idC 'x)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list '- 'y) (idC 'x)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list '* 'y) (idC 'x)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list 'ifleq0 'y) (idC 'x)))) "bad syntax")
(test/exn (check-funcs (list (fdC 'h (list 'x 'x) (idC 'x)))) "bad syntax")

;parses program for function definitions
(define (parse-prog [fundefs : s-expression]) : (listof FundefC)
  (cond
    [(s-exp-match? '(func ANY ... ANY) fundefs) (list (parse-fundef fundefs))]
    [(s-exp-list? fundefs)
     (let ([sl (s-exp->list fundefs)])
       (map parse-fundef sl))]))

(test (parse-prog '{func main x {+ 3 3}}) (list (fdC 'main (list 'x) (binop '+ (numC 3) (numC 3)))))
(test (parse-prog '{{func main {+ 3 3}} {func f x {+ 6 6}}}) (list (fdC 'main (list) (binop '+ (numC 3) (numC 3)))
                                                                     (fdC 'f (list 'x) (binop '+ (numC 6) (numC 6)))))
(test (parse-prog '{{func f x {+ x x}} {func main {f :D 1}}}) (list (fdC 'f (list 'x) (binop '+ (idC 'x) (idC 'x)))
                                                                    (fdC 'main (list) (appC 'f (list (numC 1))))))
;(test/exn (parse-prog '{hello}) "Invalid function")

;Substitution
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (map (lambda (x) (subst what for x)) a))]
    [binop (s l r) (binop s (subst what for l)
                        (subst what for r))]
    [ifleq0 (n t e) (ifleq0 (subst what for n) (subst what for t) (subst what for e))]))

(test (subst (numC 5) 'x (binop '+ (idC 'x) (numC 5))) (binop '+ (numC 5) (numC 5)))
(test (subst (numC 5) 'y (binop '+ (idC 'x) (numC 5))) (binop '+ (idC 'x) (numC 5)))
(test (subst (numC 5) 'x (ifleq0 (idC 'x) (idC 'x) (binop '+ (numC 5) (numC 5)))) (ifleq0 (numC 5) (numC 5) (binop '+ (numC 5) (numC 5))))
(test (subst (numC 5) 'x (appC 'f (list (idC 'x)))) (appC 'f (list (numC 5))))

;Find specified function in list of functions
(define (get-func [s : symbol] [funs : (listof FundefC)]) : FundefC
  (cond
    ((empty? funs) (error 'parse "Function not found"))
    (else (type-case FundefC (first funs)
            [fdC (n p b)
                 (cond
                   [(equal? n s) (first funs)]
                   [else (get-func s (rest funs))])]))))
(test/exn (get-func 'hello (list (fdC 'not-here (list 'x) (numC 5)))) "Function not found")

;Substitute for all arguments and params
(define (subst-all [args : (listof ExprC)] [params : (listof symbol)] [body : ExprC]) : ExprC
  (cond
    [(= (length args) (length params))
     (cond
       [(empty? args) body]
       [else (subst-all (rest args) (rest params) (subst (first args) (first params) body))])]
    [else (error 'parse "wrong arity")]))

;Interprets functions for evalution
(define (interp [exp : ExprC] [funs : (listof FundefC)]) : number
  (type-case ExprC exp
    [numC (n) n]
    [idC (s) (error 'parse "unbound variable")]
    [appC (f a) (type-case FundefC (get-func f funs)
                  [fdC (n p b) (interp (subst-all a p b) funs)])]
    [binop (s l r) ((operator-lookup s) (interp l funs) (interp r funs))]
    [ifleq0 (n t e) (cond
                      ((>= 0 (interp n funs)) (interp t funs))
                      (else (interp e funs)))]))

(test (interp (binop '+ (numC 5) (numC 6)) (list (fdC 'f (list 'x) (idC 'x)))) 11)
(test (interp (binop '* (numC 5) (numC 6)) (list (fdC 'f (list 'x) (idC 'x)))) 30)
(test (interp (binop '- (numC 5) (numC 6)) (list (fdC 'f (list 'x) (idC 'x)))) -1)
(test (interp (binop '/ (numC 5) (numC 5)) (list (fdC 'f (list 'x) (idC 'x)))) 1)
(test/exn (interp (binop '% (numC 5) (numC 5)) (list (fdC 'f (list 'x) (idC 'x)))) "illegal operation")
(test (interp (appC 'f (list (numC 5))) (list (fdC 'f (list 'x) (idC 'x)))) 5)
(test/exn (interp (idC 'x) (list (fdC 'f (list 'x) (idC 'x)))) "unbound variable")
(test (interp (ifleq0 (numC 5) (numC 5) (numC 6)) (list (fdC 'f (list 'x) (idC 'x)))) 6)
(test (interp (ifleq0 (numC 0) (numC 5) (numC 6)) (list (fdC 'f (list 'x) (idC 'x)))) 5)
(test/exn (interp (appC 'f (list (numC 5) (numC 6))) (list (fdC 'f (list 'x) (idC 'x)))) "wrong arity")

;Interpret program from main function
(define (interp-fns [funs : (listof FundefC)]) : number
  (type-case FundefC (get-func 'main funs)
    [fdC (n p b) (interp b funs)]))

(test (interp-fns (list (fdC 'main (list 'x) (binop '+ (numC 5) (numC 6))))) 11)
(test (interp-fns (list (fdC 'main (list) (binop '+ (numC 5) (numC 6))))) 11)

;Evalutes a function s-expression, then interprets the value
(define (top-eval [fun-exps : s-expression]) : number
  (interp-fns (parse-prog fun-exps)))

(test (top-eval '{func main {+ 3 3}}) 6)


(test (interp-fns
       (parse-prog '{{func f x {+ x x}}
                     {func main {f :D 1}}}))
      2)
(test (interp-fns
       (parse-prog '{{func f x y {+ x y}}
                     {func main {f :D 1 2}}}))
      3)
 (test (interp-fns
        (parse-prog '{{func f 5}
                      {func main {+ {f :D} {f :D}}}}))
       10)
 (test/exn (interp-fns
            (parse-prog '{{func f x y {+ x y}}
                          {func main {f :D 1}}}))
           "wrong arity")
(test (top-eval (quote ((func main (+ 1 2))))) 3)
(test (top-eval (quote ((func main (* 2 1))))) 2)
(test (top-eval (quote ((func minus x y (+ x (* -1 y))) (func main (minus :D 8 5))))) 3)
(test (top-eval (quote ((func main (seven :D)) (func seven (minus :D (+ 3 10) (* 2 3))) (func minus x y (+ x (* -1 y)))))) 7)
(test (top-eval (quote ((func main (twice :D (minus :D 8 5))) (func minus x y (+ x (* -1 y))) (func twice x (* 2 x))))) 6)
(test (top-eval (quote ((func realtwice x (+ x x)) (func main (twice :D 15)) (func twice x (realtwice :D x))))) 30)
(test (top-eval (quote ((func main (/ (- 13 3) (* 1 5)))))) 2)
(test (begin (parse-fundef (quote (func f 6))) #t) #t)
(test (top-eval (quote ((func main (ifleq0 (* 3 1) 3 (+ 2 9)))))) 11)
(test (top-eval (quote ((func main (ifleq0 (- 0 (* 3 1)) (+ 4 5) (+ 2 9)))))) 9)
(test (top-eval (quote ((func main (ifleq0 (* 3 0) 3 (+ 2 9)))))) 3)
(test (top-eval (quasiquote ((func main (f :D 9)) (func f x (g :D 3 x)) (func g a b (+ a b))))) 12)
(test (top-eval (quote ((func main (+ (f :D 13) (f :D 0))) (func f qq (ifleq0 qq qq (+ qq 1)))))) 14)
(test (top-eval (quote ((func even? x (ifleq0 x 1 (odd? :D (- x 1)))) (func odd? x (ifleq0 x 0 (even? :D (- x 1)))) (func leq? x y (ifleq0 (- x y) 1 0)) (func main (even? :D 378))))) 1)
(test (top-eval (quote ((func even? x (ifleq0 x 1 (odd? :D (- x 1)))) (func odd? x (ifleq0 x 0 (even? :D (- x 1)))) (func leq? x y (ifleq0 (- x y) 1 0)) (func main (even? :D 379))))) 0)



