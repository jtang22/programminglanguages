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
  [withC (id : ExprC) (expr : (listof ExprC))]
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
         [(s-exp-match? '(ANY = ANY) s)
          (parse (third sl))]
         [(s-exp-match? '(if ANY ANY ANY) s)
          (ifC (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(s-exp-match? '(with ANY ... ANY) s)
          (withC (parse (second sl)) (map parse (reverse (rest (reverse (rest (rest sl)))))))]
         [(s-exp-match? '(+ ANY ANY) s)
          (binop '+ (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(* ANY ANY) s)
          (binop '* (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(- ANY ANY) s)
          (binop '- (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(/ ANY ANY) s)
          (binop '/ (parse (second sl)) (parse (third sl)))]
         [(s-exp-match? '(func ANY ...) s)
          (appC (parse (first sl)) (map parse (rest (rest sl))))]
         [else (error 'parse "invalid input")]))]))

;Creates an environment based on closure params and caller args
(define (create-clos-env [params : (listof symbol)] [args : (listof ExprC)] [cloV-env : Env]) : Env
    (cond
     [(= (length params) (length args))
      (cond
        [(empty? params) cloV-env]
        [else (create-clos-env (rest params) (rest args)
                               (cons (bind (first params) (interp (first args) cloV-env)) cloV-env))])]
     [else (error 'create-clos-env "wrong arity")]))

(test/exn (create-clos-env (list) (list (numC 5)) (list)) "wrong arity")

;Interprets functions for evalution
(define (interp [exp : ExprC] [env : Env]) : Value
  (type-case ExprC exp
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [appC (f a) (type-case Value (interp f env)
                  [numV (n) (error 'interp "Number value")]
                  [cloV (p b e) (interp b (create-clos-env p a e))]
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
    [withC (i e) (error 'interp "with")]
    [ifC (c t e) (type-case Value (interp c env)
                   [numV (n) (error 'interpNum "Non boolean eval")]
                   [cloV (p b e) (error 'interpClo "Non boolean eval")]
                   [boolV (b) (cond
                                [(equal? b #t) (interp t env)]
                                [else (interp e env)])])]))

(define test-env (list))
(test (interp (numC 5) test-env) (numV 5))
(test (interp (binop '+ (numC 5) (numC 6)) test-env) (numV 11))
(test (interp (binop '- (numC 6) (numC 5)) test-env) (numV 1))
(test (interp (binop '* (binop '/ (numC 10) (numC 2)) (numC 2)) test-env) (numV 10))
(test (interp (appC (lamC (list 'x) (binop '+ (numC 5) (idC 'x))) (list (numC 5))) test-env) (numV 10))
(test/exn (interp (appC (numC 5) (list (numC 5))) test-env) "Number value")
(test/exn (interp (appC (boolC #t) (list (numC 5))) test-env) "Boolean value")
(test (interp (ifC (binop 'eq? (numC 5) (numC 5)) (numC 5) (numC 6)) test-env) (numV 5))
(test (interp (ifC (binop 'eq? (numC 4) (numC 5)) (numC 5) (numC 6)) test-env) (numV 6))
(test (interp (ifC (binop '<= (numC 5) (numC 4)) (numC 5) (numC 6)) test-env) (numV 6))
(test (interp (ifC (binop '<= (numC 4) (numC 4)) (numC 5) (numC 6)) test-env) (numV 5))
(test/exn (interp (ifC (binop '+ (numC 5) (numC 10)) (numC 1) (numC 2)) test-env) "Non boolean eval")
(test/exn (interp (ifC (appC (lamC (list 'x) (numC 5)) (list (numC 5))) (numC 1) (numC 2)) test-env) "Non boolean eval")
(test (interp (binop 'eq? (boolC #t) (boolC #t)) test-env) (boolV #t))
(test (interp (binop 'eq? (boolC #f) (boolC #t)) test-env) (boolV #f))
(test (interp (binop 'eq? (boolC #t) (appC (lamC (list 'x) (numC 5)) (list (numC 5)))) test-env) (boolV #f))
(test/exn (interp (withC (numC 5) (list (numC 5))) test-env) "with")
(test/exn (interp (ifC (numC 5) (numC 5) (numC 5)) test-env) "Non boolean eval")
(test/exn (interp (ifC (lamC (list 'x) (numC 5)) (numC 5) (numC 6)) test-env) "Non boolean eval")

(define (serialize [val : Value]) : string
  (type-case Value val
    [numV (n) (to-string n)]
    [cloV (p b e) (error 'serialize "non evaluated closure")]
    [boolV (b) (to-string b)]))

#;(define (top-eval [s : s-expression]) : string
    (serialize (interp (parse s) empty-env)))

#;(test (top-eval '{{func main {f 5}}
                    {func f x {if {<= 0 x}
                                  {with {z = {+ 9 14}} {y = 98} {+ z y}}
                                  {* {- x {/ x 2}} 2}}}}) 6)
