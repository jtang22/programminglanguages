#lang plai-typed
;LOOI3	 	=	 	num
; 	 	|	 	true
; 	 	|	 	false
; 	 	|	 	id
; 	 	|	 	{if LOOI3 LOOI3 LOOI3}
; 	 	|	 	{with {id = LOOI3} ... LOOI3}
; 	 	|	 	{func id ... LOOI3}
; 	 	|	 	{operator LOOI3 LOOI3}
; 	 	|	 	{LOOI3 LOOI3 ...}
;  operator	 	=	 	+
; 	 	|	 	-
; 	 	|	 	*
; 	 	|	 	/
; 	 	|	 	eq?
; 	 	|	 	<=

;representation of arithmetic expression
(define-type ExprC
  [numC (num : number)]
  [binop (sym : symbol) (a : ExprC) (b : ExprC)]
  ;[ifleq0 (test : ExprC) (then : ExprC) (else : ExprC)]
  [idC (x : symbol)]
  [appC (fn : symbol) (arg : (listof ExprC))]
  [lamC (param : symbol) (body : ExprC)])

;representation of values
(define-type Value
  [numV (num : number)]
  [cloV (param : symbol)
        (body : ExprC)
        (env : Env)])

;lookup an operator for a binop expression
(define (operator-lookup [s : symbol])
  (cond
    [(equal? '+ s) +]
    [(equal? '* s) *]
    [(equal? '- s) -]
    [(equal? '/ s) /]
    ;[(equal? 'eq? s) =]
    ;[(equal? '<= s) <=]
    [else (error 'parse "illegal operation")]))

;Environment
(define-type Binding
  [bind (name : symbol) (val : ExprC)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;Lookup a bound symbol in an environment
(define (lookup [for : symbol] [env : Env]) : ExprC
  (cond
    [(empty? env) (error 'parse "lookup failed")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;Interprets functions for evalution
#;(define (interp [exp : ExprC] [env : Env]) : Value
  (type-case ExprC exp
    [numC (n) n]
    [idC (s) (error 'parse "unbound variable")]
    [appC (f a) (type-case FundefC (get-func f funs)
                  [fdC (n p b) (interp (subst-all a p b) funs)])]
    [binop (s l r) ((operator-lookup s) (interp l funs) (interp r funs))]
    [ifleq0 (n t e) (cond
                      ((>= 0 (interp n funs)) (interp t funs))
                      (else (interp e funs)))]))
