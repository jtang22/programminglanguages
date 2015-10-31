#lang plai-typed
(require (typed-in racket
                   [random : (number -> number)]
                   [random-seed : (number -> void)]))
(require plai-typed/s-exp-match)

(random-seed 0)

;representation of arithmetic expression
(define-type ExprC
  [numC (num : number)]
  [boolC (bool : boolean)]
  [binop (sym : symbol) (a : ExprC) (b : ExprC)]
  [idC (x : symbol)]
  [ifC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [appC (fn : ExprC) (arg : (listof ExprC))]
  [lamC (param : (listof symbol)) (body : ExprC)])

;representation of values
(define-type Value
  [numV (num : number)]
  [cloV (param : (listof symbol))
        (body : ExprC)
        (env : Env)]
  [boolV (bool : boolean)])

;Environment bindings
(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;list of symbols
(define sList (list 'a 'b 'c 'd 'e 'f 'g' h))
;list of operators
(define opList (list '+ '- '* '/))

;Get a random symbol from a list of symbols
(define (randomSymbol [s : (listof symbol)]) : symbol
  (list-ref s (random (length s))))

(test (randomSymbol sList) 'g)

;List of expression identifiers
(define idList (list 'bool 'id 'num))

;Get a random boolean value
(define (getRandomBoolean) : boolean
  (let ([n (random 2)])
    (cond
      [(= n 0) #t]
      [(= n 1) #f])))

;Get random base term
(define (randomBaseTerm [ids : (listof symbol)]) : ExprC
  (let ([identifier (randomSymbol ids)])
    (cond
      [(eq? 'bool identifier) (boolC (getRandomBoolean))]
      [(eq? 'id identifier) (idC (randomSymbol sList))]
      [(eq? 'num identifier) (numC (random 100))])))

(test (randomBaseTerm idList) (idC 'b))
(test (randomBaseTerm idList) (idC 'h))
(test (randomBaseTerm idList) (idC 'e))
(test (randomBaseTerm idList) (idC 'c))
(test (randomBaseTerm idList) (idC 'e))
(test (randomBaseTerm idList) (boolC #f))
(test (randomBaseTerm idList) (numC 23))
(test (randomBaseTerm idList) (numC 18))
(test (randomBaseTerm idList) (boolC #f))

;Get random expression tree
(define (randomTerm [depth : number]) : ExprC
  (cond
    [(eq? depth 0) (randomBaseTerm idList)]
    [else (let ([n (random 5)])
            (cond
              [(= n 4) (randomBaseTerm idList)]
              [(= n 3) (let ([arity (random 3)])
                         (cond
                           [(= 0 arity) (appC (randomTerm (- depth 1)) (list))]
                           [(= 1 arity) (appC (randomTerm (- depth 1)) (list (randomTerm (- depth 1))))]
                           [(= 2 arity) (appC (randomTerm (- depth 1)) (list (randomTerm (- depth 1)) (randomTerm (- depth 1))))]))]
              [(= n 2) (binop (randomSymbol opList) (randomTerm (- depth 1)) (randomTerm (- depth 1)))]
              [(= n 1) (ifC (randomTerm (- depth 1)) (randomTerm (- depth 1)) (randomTerm (- depth 1)))]
              [(= n 0) (let ([arity (random 3)])
                         (cond
                           [(= 0 arity) (lamC (list) (randomTerm(- depth 1)))]
                           [(= 1 arity) (lamC (list (randomSymbol sList)) (randomTerm(- depth 1)))]
                           [(= 2 arity) (lamC (list (randomSymbol sList) (randomSymbol sList)) (randomTerm(- depth 1)))]))]))]))

(test (randomTerm 3) (lamC (list) (boolC #f)))

(test (randomTerm 5) (binop '* (appC
                                (appC
                                 (binop '- (appC
                                            (boolC #f) (list)) (boolC #f)) (list)) (list (binop '* (binop '- (lamC (list 'a) (numC 54)) (boolC #f))
                                                                                                (ifC (numC 89) (ifC (boolC #f) (idC 'f) (numC 31)) (binop '* (idC 'd) (numC 58))))))
                            (appC (ifC (lamC (list 'a 'g) (binop '* (numC 54) (boolC #t)))
                                       (idC 'e) (binop '/ (binop '- (idC 'h) (idC 'c)) (binop '+ (boolC #t) (idC 'd)))) (list))))

;Creates an environment based on closure params and caller args
;Takes a list of symbols, list of Values, and an environment
;Returns an environemnt based on param and arg bindings
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

;helper for adding
;Takes two values and adds them together, returns a value
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-num l) (numV-num r)))]
    [else (error 'num+ "one argument was not a number")]))

(test (num+ (numV 5) (numV 6)) (numV 11))
(test/exn (num+ (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")

;helper for subtracting
;Takes two values and subtracts the second from the first
;Returns a value
(define (num- [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (- (numV-num l) (numV-num r)))]
    [else (error 'num- "one argument was not a number")]))

(test (num- (numV 5) (numV 6)) (numV -1))
(test/exn (num- (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")

;helper for multiplying
;Takes two values and multiplies them
;Returns a value
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-num l) (numV-num r)))]
    [else (error 'num* "one argument was not a number")]))

(test (num* (numV 5) (numV 6)) (numV 30))
(test/exn (num* (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello (numV 5))))) "one argument was not a number")

;helper for dividing
;Takes two values and divides 
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
;Compares two values to see if they're equal
;Returns true if equal, false if not
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
;Takes two values and checks if the first is less than or equal to second
;Returns boolean true or false
(define (num<= [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (cond
       [(<= (numV-num l) (numV-num r)) (boolV #t)]
       [else (boolV #f)])]
    [else (error 'num<= "one argument was not a number")]))

(test/exn (num<= (boolV #t) (numV 5)) "one argument was not a number")

;Lookup a bound symbol in an environment (from textbook)
;Takes environment and symbol, looks for symbol in environment
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "lookup failed")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(test (lookup 's (list (bind 'h (numV 5)) (bind 's (numV 6)))) (numV 6))
(test/exn (lookup 'z (list (bind 'h (numV 5)))) "lookup failed")

;Interprets functions for evalution
;Takes an ExprC and an environment
;Returns a value based on evaluated expression
(define (interp [exp : ExprC] [env : Env]) : Value
  (type-case ExprC exp
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [appC (f a) (type-case Value (interp f env)
                  [numV (n) (error 'interp "not a closure")]
                  [cloV (p b e) (interp b (create-clos-env p (map (lambda (x) (interp x env)) a) e))]
                  [boolV (b) (error 'interp "not a closure")])]
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
(test (interp (appC (lamC (list 'x) (binop 'eq? (numC 5) (idC 'x))) (list (numC 5))) mt-env) (boolV #t))
(test (interp (ifC (binop 'eq? (numC 5) (numC 5)) (numC 5) (numC 6)) mt-env) (numV 5))
(test (interp (ifC (binop 'eq? (numC 4) (numC 5)) (numC 5) (numC 6)) mt-env) (numV 6))
(test (interp (ifC (binop '<= (numC 5) (numC 4)) (numC 5) (numC 6)) mt-env) (numV 6))
(test (interp (ifC (binop '<= (numC 4) (numC 4)) (numC 5) (numC 6)) mt-env) (numV 5))
(test/exn (interp (ifC (binop '+ (numC 5) (numC 10)) (numC 1) (numC 2)) mt-env) "Non boolean eval")
(test/exn (interp (ifC (appC (lamC (list 'x) (numC 5)) (list (numC 5))) (numC 1) (numC 2)) mt-env) "Non boolean eval")
(test/exn (interp (appC (numC 5) (list)) mt-env) "not a closure")
(test/exn (interp (appC (boolC #t) (list)) mt-env) "not a closure")
(test (interp (binop 'eq? (boolC #t) (boolC #t)) mt-env) (boolV #t))
(test (interp (binop 'eq? (boolC #f) (boolC #t)) mt-env) (boolV #f))
(test (interp (binop 'eq? (boolC #t) (appC (lamC (list 'x) (numC 5)) (list (numC 5)))) mt-env) (boolV #f))
(test/exn (interp (ifC (numC 5) (numC 5) (numC 5)) mt-env) "Non boolean eval")
(test/exn (interp (ifC (lamC (list 'x) (numC 5)) (numC 5) (numC 6)) mt-env) "Non boolean eval")

(interp (randomTerm 4) mt-env)
(interp (randomTerm 3) mt-env)

;Tests errors for a number of trials
(define (errorTrials [trials : number] [depth : number] [errors : number]) : number
  (let ([num (try (begin (interp (randomTerm depth) mt-env) 1) (lambda() 0))])
    (cond
      [(= 0 trials) errors]
      [else
        (cond
          [(= num 0) (errorTrials (- trials 1) depth (+ 1 errors))]
          [else (errorTrials (- trials 1) depth errors)])])))

;Runs trials
(define (runTrials [trials : number] [depth : number]) : number
  (/ (errorTrials trials depth 0) trials))

(runTrials 2 3)
(runTrials 4 5)
(runTrials 5 8)
(runTrials 1000 7)