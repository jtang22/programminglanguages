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

;representation of arithmetic function for LOOI2
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
         [(s-exp-match? '(SYMBOL :D ANY ...) s)
          (appC (s-exp->symbol (first sl)) (parse (third sl)))]
         [else (error 'parse "invalid input")]))]))

(test (parse '(ifleq0 5 5 (+ 6 5))) (ifleq0 (numC 5) (numC 5) (binop '+ (numC 6) (numC 5))))
(test (parse '(+ 5 5)) (binop '+ (numC 5) (numC 5)))
(test (parse '(* 5 5)) (binop '* (numC 5) (numC 5)))
(test (parse '(- 5 5)) (binop '- (numC 5) (numC 5)))
(test (parse '(/ 5 5)) (binop '/ (numC 5) (numC 5)))
(test/exn (parse '(+ 5 5 5)) "invalid input")
(test (parse '(f :D 5)) (appC 'f (numC 5)))
(test (parse '(f)) (idC 'f))
(test (parse '(5)) (numC 5))

;parser for function definitions
(define (parse-fundef [fundef : s-expression]) : FundefC
  (cond
    [(s-exp-list? fundef)
     (let ([sl (s-exp->list fundef)])
       (cond
         [(s-exp-match? '(func SYMBOL ... ANY) fundef)
          (fdC (s-exp->symbol (second sl))
               (s-exp->symbol (third sl))
               (parse (fourth sl)))]
         [else (error 'parse "invalid input")]))]))

(test (parse-fundef '(func hello x (+ x x))) (fdC 'hello 'x (binop '+ (idC 'x) (idC 'x))))
(test/exn (parse-fundef '(func)) "invalid input")

;parses program for function definitions
(define (parse-prog [fundefs : s-expression]) : (listof FundefC)
  (cond
    [(s-exp-match? '(func SYMBOL ANY ANY) fundefs) (list (parse-fundef fundefs))]))
;    [(s-exp-list? fundefs)
;     (let ([sl (s-exp->list fundefs)])
;       (cons (parse-fundef (first sl)) (parse-prog (list->s-exp (rest sl)))))]))

(test (parse-prog '{func main x {+ 3 3}}) (list (fdC 'main 'x (binop '+ (numC 3) (numC 3)))))
#;(test (parse-prog '{{func main x {+ 3 3}} {func f x {+ 6 6}}}) (list (fdC 'main 'x (binop '+ (numC 3) (numC 3)))
                                                                     (fdC 'f 'x (binop '+ (numC 6) (numC 6)))))

;Substitution
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [binop (s l r) (binop s (subst what for l)
                        (subst what for r))]
    [ifleq0 (n t e) (ifleq0 (subst what for n) (subst what for t) (subst what for e))]))

(test (subst (numC 5) 'x (binop '+ (idC 'x) (numC 5))) (binop '+ (numC 5) (numC 5)))
(test (subst (numC 5) 'y (binop '+ (idC 'x) (numC 5))) (binop '+ (idC 'x) (numC 5)))
(test (subst (numC 5) 'x (ifleq0 (idC 'x) (idC 'x) (binop '+ (numC 5) (numC 5)))) (ifleq0 (numC 5) (numC 5) (binop '+ (numC 5) (numC 5))))
(test (subst (numC 5) 'x (appC 'f (idC 'x))) (appC 'f (numC 5)))

;Find specified function in list of functions
(define (get-func [s : symbol] [funs : (listof FundefC)]) : FundefC
  (cond
    ((empty? funs) (error 'parse "Function not found"))
    (else (type-case FundefC (first funs)
            [fdC (n p b)
                 (cond
                   [(equal? n s) (first funs)]
                   [else (get-func s (rest funs))])]))))
(test/exn (get-func 'hello (list (fdC 'not-here 'x (numC 5)))) "Function not found")

;Interprets functions for evalution
(define (interp [exp : ExprC] [funs : (listof FundefC)]) : number
  (type-case ExprC exp
    [numC (n) n]
    [idC (s) (error 'parse "unbound variable")]
    [appC (f a) (type-case FundefC (get-func f funs)
                  [fdC (n p b) (interp (subst a p b) funs)])]
    [binop (s l r) ((operator-lookup s) (interp l funs) (interp r funs))]
    [ifleq0 (n t e) (cond
                      ((= 0 (interp n funs)) (interp t funs))
                      (else (interp e funs)))]))

(test (interp (binop '+ (numC 5) (numC 6)) (list (fdC 'f 'x (idC 'x)))) 11)
(test (interp (binop '* (numC 5) (numC 6)) (list (fdC 'f 'x (idC 'x)))) 30)
(test (interp (binop '- (numC 5) (numC 6)) (list (fdC 'f 'x (idC 'x)))) -1)
(test (interp (binop '/ (numC 5) (numC 5)) (list (fdC 'f 'x (idC 'x)))) 1)
(test/exn (interp (binop '% (numC 5) (numC 5)) (list (fdC 'f 'x (idC 'x)))) "illegal operation")
(test (interp (appC 'f (numC 5)) (list (fdC 'f 'x (idC 'x)))) 5)
(test/exn (interp (idC 'x) (list (fdC 'f 'x (idC 'x)))) "unbound variable")
(test (interp (ifleq0 (numC 5) (numC 5) (numC 6)) (list (fdC 'f 'x (idC 'x)))) 6)
(test (interp (ifleq0 (numC 0) (numC 5) (numC 6)) (list (fdC 'f 'x (idC 'x)))) 5)
;Interpret program from main function
(define (interp-fns [funs : (listof FundefC)]) : number
  (type-case FundefC (get-func 'main funs)
    [fdC (n p b) (interp b funs)]))
(test (interp-fns (list (fdC 'main 'x (binop '+ (numC 5) (numC 6))))) 11)

(define (top-eval [fun-exps : s-expression]) : number
  (interp-fns (parse-prog fun-exps)))

(test (top-eval '{func main x {+ 3 3}}) 6)


;(test (interp-fns
;       (parse-prog '{{func f x {+ x x}}
;                     {func main {f :D 1}}}))
;      2)
;(test (interp-fns
;       (parse-prog '{{func f x y {+ x y}}
;                     {func main {f :D 1 2}}}))
;      3)
; (test (interp-fns
;        (parse-prog '{{func f 5}
;                      {func main {+ {f :D} {f :D}}}}))
;       10)
; (test/exn (interp-fns
;            (parse-prog '{{func f x y {+ x y}}
;                          {func main {f :D 1}}}))
;           "wrong arity")

