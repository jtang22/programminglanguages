#lang plai-typed
(require plai-typed/s-exp-match)
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
  [boolC (bool : boolean)]
  [binop (sym : symbol) (a : ExprC) (b : ExprC)]
  [idC (x : symbol)]
  [ifC (cond : ExprC) (then : ExprC) (else : ExprC)]
  ;[withC (id : Env) (expr : ExprC)]
  [appC (fn : ExprC) (arg : (listof ExprC))]
  [lamC (param : (listof symbol)) (body : ExprC)])

;representation of values
(define-type Value
  [numV (num : number)]
  [cloV (param : (listof symbol))
        (body : ExprC)
        (env : Env)]
  [boolV (bool : boolean)])

;Environment
(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;helper for adding
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-num l) (numV-num r)))]
    [else (error 'num+ "one argument was not a number")]))

(test (num+ (numV 5) (numV 6)) (numV 11))
(test/exn (num+ (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")

;helper for subtracting
(define (num- [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (- (numV-num l) (numV-num r)))]
    [else (error 'num- "one argument was not a number")]))

(test (num- (numV 5) (numV 6)) (numV -1))
(test/exn (num- (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")

;helper for multiplying
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-num l) (numV-num r)))]
    [else (error 'num* "one argument was not a number")]))

(test (num* (numV 5) (numV 6)) (numV 30))
(test/exn (num* (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")

;helper for dividing
(define (num/ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (cond
       [(= 0 (numV-num r)) (error 'num/ "divide by zero")]
       [else (numV (/ (numV-num l) (numV-num r)))])]
    [else (error 'num/ "one argument was not a number")]))

(test (num/ (numV 10) (numV 5)) (numV 2))
(test/exn (num/ (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")
(test/exn (num/ (numV 10) (numV 0)) "divide by zero")

;helper function for equality
(define (numEq [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (cond
       [(= (numV-num r) (numV-num l)) (boolV #t)]
       [else (boolV #f)])]
    [(and (boolV? l) (boolV? r))
     (cond
       [(equal? (boolV-bool l) (boolV-bool r)) (boolV #t)]
       [else (boolV #f)])]
    [else (boolV #f)]))

;helper function for less than or equal to
(define (num<= [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (cond
       [(<= (numV-num l) (numV-num r)) (boolV #t)]
       [else (boolV #f)])]
    [else (error 'num<= "one argument was not a number")]))

(test/exn (num<= (boolV #t) (numV 5)) "one argument was not a number")

;Lookup a bound symbol in an environment
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'parse "lookup failed")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(test (lookup 's (list (bind 'h (numV 5)) (bind 's (numV 6)))) (numV 6))
(test/exn (lookup 'z (list (bind 'h (numV 5)))) "lookup failed")

;Create list of params for with function
(define (create-func-lam [sl : (listof s-expression)]) : (listof symbol)
  (cond
    [(empty? sl) empty]
    [(s-exp-match? '(ANY = ANY) (first sl))
     (cons (s-exp->symbol (first (s-exp->list (first sl))))
           (create-func-lam (rest sl)))]
    [else (error 'create-func-lam "invalid format")]))

;Create list of args for with call
(define (create-func-args [sl : (listof s-expression)]) : (listof ExprC)
  (cond
    [(empty? sl) empty]
    [(s-exp-match? '(ANY = ANY) (first sl))
     (cons (parse (third (s-exp->list (first sl)))) (create-func-args (rest sl)))]
    [else (error 'create-func-args "invalid format")]))

(test/exn (create-func-args (list '{x})) "invalid format")

;Give list of symbols to check for dups
(define (check-params [params : (listof symbol)]) : boolean
  (cond
    [(empty? params) #t]
    [(equal? #t (member (first params) (rest params))) #f]
    [else (and #t (check-params (rest params)))]))

;Evaluates an s-expression and returns as an ExprC
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (cond
                         [(equal? '+ (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? '- (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? '* (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? '/ (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? 'eq? (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? '<= (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? 'if (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [(equal? 'with (s-exp->symbol s)) (error 'parse "invalid parameter name")]
                         [else (idC (s-exp->symbol s))])]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-match? '(NUMBER) s)
          (parse(first sl))]
         [(s-exp-boolean? (first sl))
          (parse (first sl))]
         [(s-exp-match? '(SYMBOL) s)
          (parse (first sl))]
         [(s-exp-match? '(if ANY ANY ANY) s)
          (ifC (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(s-exp-match? '(with ANY ... ANY) s)
          (let ([params (create-func-lam (reverse (rest (reverse (rest sl)))))])
            (cond
              [(equal? #t (check-params params))
               (appC (lamC params (parse (first (reverse sl))))
                (create-func-args (reverse (rest (reverse (rest sl))))))]
              [else (error 'parse "Duplicate with params")]))]
         [(s-exp-match? '(+ ANY ANY) s)
          (binop '+ (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(* ANY ANY) s)
          (binop '* (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(- ANY ANY) s)
          (binop '- (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(/ ANY ANY) s)
          (binop '/ (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(eq? ANY ANY) s)
          (binop 'eq? (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(<= ANY ANY) s)
          (binop '<= (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(func ANY ...) s)
          (let ([params (map s-exp->symbol (reverse (rest (reverse (rest sl)))))])
           (cond
            [(equal? #t (check-params params))
             (lamC params (parse (first (reverse sl))))]
            [else (error 'parse "Duplicate params")]))]
         [(s-exp-match? '(ANY ...) s)
          (appC (parse (first sl)) (map parse (rest sl)))]))]))

(test (parse '{with {x = 5} {+ x 5}}) (appC (lamC (list 'x) (binop '+ (idC 'x) (numC 5))) (list (numC 5))))
(test (parse '{with {x = 5} {y = 6} {+ x y}}) (appC (lamC (list 'x 'y) (binop '+ (idC 'x) (idC 'y))) (list (numC 5) (numC 6))))
(test (parse '{+ 5 3}) (binop '+ (numC 5) (numC 3)))
(test (parse '{- 5 3}) (binop '- (numC 5) (numC 3)))
(test (parse '{* 5 3}) (binop '* (numC 5) (numC 3)))
(test (parse '{/ 6 3}) (binop '/ (numC 6) (numC 3)))
(test (parse '{eq? 5 5}) (binop 'eq? (numC 5) (numC 5)))
(test (parse '{<= 5 6}) (binop '<= (numC 5) (numC 6)))
(test (parse '{#t}) (boolC #t))
(test (parse '{#f}) (boolC #f))
(test (parse '{eq? #t #f}) (binop 'eq? (boolC #t) (boolC #f)))
(test (parse '{func x y {+ x y}}) (lamC (list 'x 'y) (binop '+ (idC 'x) (idC 'y))))
(test (parse '{with {x = {+ 5 4}} {+ x 1}}) (appC (lamC (list 'x) (binop '+ (idC 'x) (numC 1))) (list (binop '+ (numC 5) (numC 4)))))
(test (parse '{1}) (numC 1))
(test (parse '{#t}) (boolC #t))
(test (parse '{if {eq? x 5} {x} {- x 5}}) (ifC (binop 'eq? (idC 'x) (numC 5)) (idC 'x) (binop '- (idC 'x)(numC 5))))
#;(test (parse '(quote ((func seven (seven))
                  ((func minus
                         (func (minus (+ 3 10) (* 2 3))))
                   (func x y (+ x (* -1 y))))))))
(test/exn (parse '{with {x} {+ x 5}}) "invalid format")
(test/exn (parse '{+}) "invalid parameter name")
(test/exn (parse '{-}) "invalid parameter name")
(test/exn (parse '{/}) "invalid parameter name")
(test/exn (parse '{*}) "invalid parameter name")
(test/exn (parse '{eq?}) "invalid parameter name")
(test/exn (parse '{<=}) "invalid parameter name")
(test/exn (parse '{if}) "invalid parameter name")
(test/exn (parse '{with}) "invalid parameter name")
(test/exn (parse '{func x x {+ x x}}) "Duplicate params")
(test/exn (parse '{with {x = 5} {x = 5} {+ x x}}) "Duplicate with params")


;Creates an environment based on closure params and caller args
(define (create-clos-env [params : (listof symbol)] [args : (listof Value)] [cloV-env : Env]) : Env
    (cond
     [(= (length params) (length args))
      (cond
        [(empty? params) cloV-env]
        [else (create-clos-env (rest params) (rest args)
                               (cons (bind (first params) (first args)) cloV-env))])]
     [else (error 'create-clos-env "wrong arity")]))

(test (create-clos-env (list 'seven) (list (numV 7)) mt-env) (list (bind 'seven (numV 7))))
(test/exn (create-clos-env (list) (list (numV 5)) (list)) "wrong arity")

;Interprets functions for evalution
(define (interp [exp : ExprC] [env : Env]) : Value
  (type-case ExprC exp
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [appC (f a) (type-case Value (interp f env)
                  [numV (n) (error 'interp "Number value")]
                  [cloV (p b e) (interp b (create-clos-env p (map (lambda (x) (interp x env)) a) e))]
                  [boolV (b) (error 'interp "Boolean value")])]
    [binop (s l r) (cond
                      [(equal? '+ s) (num+ (interp l env) (interp r env))]
                      [(equal? '- s) (num- (interp l env) (interp r env))]
                      [(equal? '* s) (num* (interp l env) (interp r env))]
                      [(equal? '/ s) (num/ (interp l env) (interp r env))]
                      [(equal? 'eq? s) (numEq (interp l env) (interp r env))]
                      [(equal? '<= s) (num<= (interp l env) (interp r env))])]
    [lamC (p b) (cloV p b env)]
    [boolC (b) (cond
                 [(equal? b #t) (boolV #t)]
                 [else (boolV #f)])]
    [ifC (c t e) (type-case Value (interp c env)
                   [numV (n) (error 'interpNum "Non boolean eval")]
                   [cloV (p b e) (error 'interpClo "Non boolean eval")]
                   [boolV (b) (cond
                                [(equal? b #t) (interp t env)]
                                [else (interp e env)])])]))

(test (interp (numC 5) mt-env) (numV 5))
(test (interp (binop '+ (numC 5) (numC 6)) mt-env) (numV 11))
(test (interp (binop '- (numC 6) (numC 5)) mt-env) (numV 1))
(test (interp (binop '* (binop '/ (numC 10) (numC 2)) (numC 2)) mt-env) (numV 10))
(test (interp (appC (lamC (list 'x) (binop '+ (numC 5) (idC 'x))) (list (numC 5))) mt-env) (numV 10))
(test/exn (interp (appC (numC 5) (list (numC 5))) mt-env) "Number value")
(test/exn (interp (appC (boolC #t) (list (numC 5))) mt-env) "Boolean value")
(test (interp (ifC (binop 'eq? (numC 5) (numC 5)) (numC 5) (numC 6)) mt-env) (numV 5))
(test (interp (ifC (binop 'eq? (numC 4) (numC 5)) (numC 5) (numC 6)) mt-env) (numV 6))
(test (interp (ifC (binop '<= (numC 5) (numC 4)) (numC 5) (numC 6)) mt-env) (numV 6))
(test (interp (ifC (binop '<= (numC 4) (numC 4)) (numC 5) (numC 6)) mt-env) (numV 5))
(test/exn (interp (ifC (binop '+ (numC 5) (numC 10)) (numC 1) (numC 2)) mt-env) "Non boolean eval")
(test/exn (interp (ifC (appC (lamC (list 'x) (numC 5)) (list (numC 5))) (numC 1) (numC 2)) mt-env) "Non boolean eval")
(test (interp (binop 'eq? (boolC #t) (boolC #t)) mt-env) (boolV #t))
(test (interp (binop 'eq? (boolC #f) (boolC #t)) mt-env) (boolV #f))
(test (interp (binop 'eq? (boolC #t) (appC (lamC (list 'x) (numC 5)) (list (numC 5)))) mt-env) (boolV #f))
(test/exn (interp (ifC (numC 5) (numC 5) (numC 5)) mt-env) "Non boolean eval")
(test/exn (interp (ifC (lamC (list 'x) (numC 5)) (numC 5) (numC 6)) mt-env) "Non boolean eval")
(interp (appC (lamC (list 'hello) (idC 'hello)) (list (lamC (list) (binop '+ (numC 5) (numC 5))))) mt-env)
(interp (appC (lamC (list 'seven) (idC 'seven)) (list (lamC (list) (numC 7)))) mt-env)
(interp (appC (lamC (list 'seven) (idC 'seven)) (list (lamC (list 'five) (numC 7)))) mt-env)

;Changes value into a string
(define (serialize [val : Value]) : string
  (type-case Value val
    [numV (n) (to-string n)]
    [cloV (p b e) "#<procedure>"]
    [boolV (b) (to-string b)]))
(test (serialize (numV 5)) "5")
(test (serialize (boolV #t)) "#t")

;Evaluates a given s-expression
(define (top-eval [s : s-expression]) : string
    (serialize (interp (parse s) mt-env)))

(test (top-eval '{+ 5 3}) "8")
(test (top-eval '{{func x {+ x 3}} 3}) "6")

(test (top-eval (quote ((func seven (seven))
                  ((func minus
                         (func (minus (+ 3 10) (* 2 3))))
                   (func x y (+ x (* -1 y))))))) "7")

(test (top-eval (quote ((func seven (seven))
                  (func 7)))) "7")
(parse (quote ((func seven (seven))
               (func 7))))
(interp (parse (quote ((func seven (seven))
               (func 7)))) mt-env)
(parse (quote ((func seven (seven))
                  ((func minus
                         (func (minus (+ 3 10) (* 2 3))))
                   (func x y (+ x (* -1 y)))))))
(interp (parse (quote ((func seven (seven))
                  ((func minus
                         (func (minus (+ 3 10) (* 2 3))))
                   (func x y (+ x (* -1 y))))))) mt-env)