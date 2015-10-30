#lang plai-typed
(require plai-typed/s-exp-match)
;  LOOI4	 	=	 	num
; 	 	|	 	true
; 	 	|	 	false
; 	 	|	 	id
; 	 	|	 	{new-array LOOI4 LOOI4}
; 	 	|	 	{array LOOI4 ...}
; 	 	|	 	{ref LOOI4[LOOI4]}
; 	 	|	 	{LOOI4[LOOI4] <- LOOI4}
; 	 	|	 	{id <- LOOI4}
; 	 	|	 	{begin LOOI4 LOOI4 ...}
; 	 	|	 	{if LOOI4 LOOI4 LOOI4}
; 	 	|	 	{with {id = LOOI4} ... LOOI4}
; 	 	|	 	{func id ... LOOI4}
; 	 	|	 	{operator LOOI4 LOOI4}
; 	 	|	 	{LOOI4 LOOI4 ...}

;representation of arithmetic expression
(define-type ExprC
  [numC (num : number)]
  [boolC (bool : boolean)]
  [binop (sym : symbol) (a : ExprC) (b : ExprC)]
  [idC (x : symbol)]
  [ifC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [appC (fn : ExprC) (arg : (listof ExprC))]
  [lamC (param : (listof symbol)) (body : ExprC)]
  [arrayC (size : number) (elements : (listof ExprC))]
  [beginC (evals : (listof ExprC)) (val : ExprC)]
  [refC (name : ExprC) (loc : ExprC)]
  [setArrC (name : ExprC) (ndx : ExprC) (val : ExprC)]
  [setMutC (name : ExprC) (val : ExprC)]
  #;[new-array (elements : (listof ExprC))])

(define-type-alias Location number)

;representation of values
(define-type Value
  [numV (num : number)]
  [cloV (param : (listof symbol))
        (body : ExprC)
        (env : Env)]
  [boolV (bool : boolean)]
  [arrayV (size : number)
          (base : Location)])

;Environment bindings
(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;Storage
(define-type Storage
  [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

;Result
(define-type Result
  [v*s (v : Value) (s : Store)])

;Allocation result
(define-type Alloc
  [loc*sto (base : Location) (s : Store)])

;Environment and Store
(define-type Lookups
  [env*sto (env : Env) (s : Store)])

;Lookup a bound symbol in an environment (from textbook)
;Takes environment and symbol, looks for symbol in environment
(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "lookup failed")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(test (lookup 's (list (bind 'h 5) (bind 's 6))) 6)
(test/exn (lookup 'z (list (bind 'h 5))) "lookup failed")

;Fetch data from memory location
(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "out of bounds")]
    [(eq? loc (cell-location (first sto))) (cell-val (first sto))]
    [else (fetch loc (rest sto))]))

(test (fetch 2 (list (cell 4 (numV 8))
                     (cell 3 (numV 7))
                     (cell 2 (boolV #t))
                     (cell 1 (numV 6))
                     (cell 0 (numV 5)))) (boolV #t))
(test/exn (fetch 5 (list (cell 4 (numV 8))
                     (cell 3 (numV 7))
                     (cell 2 (boolV #t))
                     (cell 1 (numV 6))
                     (cell 0 (numV 5)))) "out of bounds")

;helper for adding
;Takes two values and adds them together, returns a value
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-num l) (numV-num r)))]
    [else (error 'num+ "one argument was not a number")]))

(test (num+ (numV 5) (numV 6)) (numV 11))
(test/exn (num+ (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello 5)))) "one argument was not a number")

;helper for subtracting
;Takes two values and subtracts the second from the first
;Returns a value
(define (num- [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (- (numV-num l) (numV-num r)))]
    [else (error 'num- "one argument was not a number")]))

(test (num- (numV 5) (numV 6)) (numV -1))
(test/exn (num- (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello 5)))) "one argument was not a number")

;helper for multiplying
;Takes two values and multiplies them
;Returns a value
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-num l) (numV-num r)))]
    [else (error 'num* "one argument was not a number")]))

(test (num* (numV 5) (numV 6)) (numV 30))
(test/exn (num* (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello 5)))) "one argument was not a number")

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
(test/exn (num/ (numV 5) (cloV (list 's) (idC 'x) (list (bind 'hello 5)))) "one argument was not a number")
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
(test (numEq (cloV (list) (numC 5) mt-env) (cloV (list) (numC 5) mt-env)) (boolV #f))

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

;Creates a list for a new array
(define (create-new-array [num : number] [val : ExprC]) : (listof ExprC)
  (cond
   [(eq? num 0) empty]
   [else (cons val (create-new-array (- num 1) val))]))

(test (create-new-array 3 (numC 5)) (list (numC 5) (numC 5) (numC 5)))

;Evaluates an s-expression and returns as an ExprC
;Takes an s-expression
;Returns an ExprC, parsed s-expression
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
                         [(equal? 'true (s-exp->symbol s)) (boolC #t)]
                         [(equal? 'false (s-exp->symbol s)) (boolC #f)]
                         [else (idC (s-exp->symbol s))])]
    #;[(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-number? (first sl))
          (parse (first sl))]
         #;[(s-exp-boolean? (first sl))
          (parse (first sl))]
         #;[(s-exp-match? '(if ANY ANY ANY) s)
          (ifC (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         #;[(s-exp-match? '(with ANY ... ANY) s)
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
         [(s-exp-match? '(new-array ANY ANY) s)
          (let ([num (s-exp->number (second sl))])
            (arrayC num (create-new-array num (parse (third sl)))))]
         [(s-exp-match? '(array ANY ...) s)
          (let ([num (length (rest sl))])
            (arrayC num (map parse (rest sl))))]
         [(s-exp-match? '(begin ANY ANY ...) s)
          (beginC (map parse (reverse (rest (reverse (rest sl))))) (parse (first (reverse sl))))]
         [(s-exp-match? '(SYMBOL <- ANY) s)
          (setMutC (parse (first sl)) (parse (third sl)))]
         [(s-exp-match? '(ANY [ANY] <- ANY) s)
          (setArrC (parse (first sl)) (parse (second sl)) (parse (fourth sl)))]
         [(s-exp-match? '(ref ANY [ANY]) s)
          (refC (parse (second sl)) (parse (third sl)))]
         #;[(s-exp-match? '(func SYMBOL ... ANY) s)
          (let ([params (map s-exp->symbol (rest (reverse (rest sl))))])
           (cond
            [(equal? #t (check-params params))
             (lamC params (parse (first (reverse sl))))]
            [else (error 'parse "Duplicate params")]))]
         [(s-exp-match? '(ANY ANY ...) s)
          (appC (parse (first sl)) (map parse (rest sl)))]))]))

(test (parse '{+ 5 3}) (binop '+ (numC 5) (numC 3)))
(test (parse '{- 5 3}) (binop '- (numC 5) (numC 3)))
(test (parse '{* 5 3}) (binop '* (numC 5) (numC 3)))
(test (parse '{/ 6 3}) (binop '/ (numC 6) (numC 3)))
(test (parse '{eq? 5 5}) (binop 'eq? (numC 5) (numC 5)))
(test (parse '{<= 5 6}) (binop '<= (numC 5) (numC 6)))
(test (parse '{true}) (appC (boolC #t) (list)))
(test (parse '{false}) (appC (boolC #f) (list)))
(test (parse '{hello}) (appC (idC 'hello) (list)))
(test (parse '{array 3 14 false 5}) (arrayC 4 (list (numC 3) (numC 14) (boolC #f) (numC 5))))
(test (parse '{new-array 3 0.0}) (arrayC 3 (list (numC 0.0) (numC 0.0) (numC 0.0))))
(test (parse '{begin {f 9} p}) (beginC (list (appC (idC 'f) (list (numC 9)))) (idC 'p)))
(test (parse '{ref p [15]}) (refC (idC 'p) (numC 15)))
(test (parse '{p [15] <- {f 6}}) (setArrC (idC 'p) (numC 15) (appC (idC 'f) (list (numC 6)))))
(test (parse '{l <- 9}) (setMutC (idC 'l) (numC 9)))
(test/exn (parse '{+}) "invalid parameter name")
(test/exn (parse '{-}) "invalid parameter name")
(test/exn (parse '{/}) "invalid parameter name")
(test/exn (parse '{*}) "invalid parameter name")
(test/exn (parse '{eq?}) "invalid parameter name")
(test/exn (parse '{<=}) "invalid parameter name")
(test/exn (parse '{if}) "invalid parameter name")
(test/exn (parse '{with}) "invalid parameter name")

;Interprets functions for evalution
;Takes an ExprC and an environment
;Returns a value based on evaluated expression
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC exp
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
    #;[appC (f a) (type-case Result (interp f env sto)
                    [v*s (v-f s-f)
                         (let ([args (map (lambda(x) (interp x env sto)) a)])
                           (type-case Lookups (add-env-and-sto
                                               env sto (cloV-param v-f) a)
                             [env*sto (interp (cloV-body v-f)())]))
                         #;(type-case Value val
                           [numV (n) (error 'interp "not a closure")]
                           [cloV (p b e) (interp b (create-clos-env p (map (lambda (x) (interp x env sto)) a) e) sto)]
                           [boolV (b) (error 'interp "not a closure")])])]
    [binop (s l r) (cond
                      [(equal? '+ s) (type-case Result (interp l env sto)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp r env s-l)
                                              [v*s (v-r s-r)
                                                   (v*s (num+ v-l v-r) s-r)])])]
                      [(equal? '- s) (type-case Result (interp l env sto)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp r env sto)
                                              [v*s (v-r s-r)
                                                   (v*s (num- v-l v-r) s-r)])])]
                      [(equal? '* s) (type-case Result (interp l env sto)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp r env sto)
                                              [v*s (v-r s-r)
                                                   (v*s (num* v-l v-r) s-r)])])]
                      [(equal? '/ s) (type-case Result (interp l env sto)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp r env sto)
                                              [v*s (v-r s-r)
                                                   (v*s (num/ v-l v-r) s-r)])])]
                      [(equal? 'eq? s) (type-case Result (interp l env sto)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp r env sto)
                                              [v*s (v-r s-r)
                                                   (v*s (numEq v-l v-r) s-r)])])]
                      [(equal? '<= s) (type-case Result (interp l env sto)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp r env sto)
                                              [v*s (v-r s-r)
                                                   (v*s (num<= v-l v-r) s-r)])])])]
    #;[lamC (p b) (cloV p b env)]
    [boolC (b) (cond
                 [(equal? b #t) (v*s (boolV #t) sto)]
                 [else (v*s (boolV #f) sto)])]
    #;[ifC (c t e) (type-case Value (interp c env sto)
                   [numV (n) (error 'interpNum "Non boolean eval")]
                   [cloV (p b e) (error 'interpClo "Non boolean eval")]
                   [boolV (b) (cond
                                [(equal? b #t) (interp t env sto)]
                                [else (interp e env sto)])])]
    [arrayC (size e) (type-case Alloc (allocate sto (map (lambda(x) (interp x env sto)) e) -1)
                       [loc*sto (l s) (v*s (arrayV size l) s)])]
    [refC (name loc) (type-case Result (interp name env sto)
                       [v*s (v-l s-l)
                            (type-case Result (interp loc env s-l)
                              [v*s (v-r s-r)
                                   (v*s (fetch (numV-num v-r) s-r) s-r)])])]
    #;[beginC (e v) (interp v env (eval-begin e))]
    [else (error 'interp "not implemented")]))

(test (interp (binop '+ (numC 5) (numC 6)) mt-env mt-store) (v*s (numV 11) mt-store))
(test (interp (binop '- (numC 6) (numC 5)) mt-env mt-store) (v*s (numV 1) mt-store))
(test (interp (binop '* (binop '/ (numC 10) (numC 2)) (numC 2)) mt-env mt-store) (v*s (numV 10) mt-store))
(test (interp (binop '/ (numC 10) (numC 5)) mt-env mt-store) (v*s (numV 2) mt-store))
(test (interp (binop '<= (numC 10) (numC 11)) mt-env mt-store) (v*s (boolV #t) mt-store))
(test (interp (binop '<= (numC 10) (numC 9)) mt-env mt-store) (v*s (boolV #f) mt-store))
(test (interp (binop 'eq? (boolC #t) (boolC #t)) mt-env mt-store) (v*s (boolV #t) mt-store))
(test (interp (binop 'eq? (boolC #f) (boolC #t)) mt-env mt-store) (v*s (boolV #f) mt-store))
(test (interp (binop 'eq? (numC 5) (numC 6)) mt-env mt-store) (v*s (boolV #f) mt-store))
(test (interp (binop 'eq? (numC 5) (numC 5)) mt-env mt-store) (v*s (boolV #t) mt-store))
(test/exn (interp (ifC (boolC #t) (numC 5) (numC 6)) mt-env mt-store) "not implemented")

;Finds next open location in storage
(define (find-next-base [sto : Store] [cur-loc : Location]) : Location
  (cond
    [(empty? sto) (+ 1 cur-loc)]
    [else (type-case Storage (first sto)
    [cell (l v) (cond
                  [(> l cur-loc) (find-next-base (rest sto) l)]
                  [else (find-next-base (rest sto) cur-loc)])])]))

;Allocates new location for array
(define (allocate [sto : Store] [val : (listof Result)] [base-loc : Location]) : Alloc
  (cond
   [(= base-loc -1) (let ([base (find-next-base sto base-loc)])(allocate
                    (cons (cell base (v*s-v (first val))) sto) (rest val) base))]
   [(empty? val) (loc*sto base-loc sto)]
   [else (allocate (cons (cell (find-next-base sto base-loc) (v*s-v(first val))) sto) (rest val) base-loc)]))

;Gets an array from a store
(define (get-array [sto : Store] [array : Value] [size : number]) : (listof Result)
  (cond
    [(= (arrayV-size array) size) empty]
    [else (cons (v*s (fetch (+ (arrayV-base array) size) sto) sto)
                (get-array sto array (+ 1 size)))]))

;Adds on to environment and store for an application
(define (add-env-and-sto [env : Env] [sto : Store] [params : (listof symbol)] [args : (listof ExprC)]) : Lookups
  (cond
    [(empty? params)
     (cond
       [(empty? args) (env*sto env sto)])]
    [(= (length env) (length sto))
     (type-case Result (interp (first args) env sto)
       [v*s (v-f s-f)
            (type-case Value v-f
              [arrayV (size b)
                      (type-case Alloc (allocate sto (get-array s-f v-f 0) -1)
                        [loc*sto (l-a s-a) (add-env-and-sto
                                        (extend-env (bind (first params) l-a) env)
                                        s-a (rest params) (rest args))])]
              [else (let ([where (find-next-base sto -1)])
                      (add-env-and-sto (extend-env (bind (first params) where) env)
                                       (override-store (cell where v-f) sto)
                                       (rest params) (rest args)))])])]
    [else (error 'add-env-and-sto "wrong arity")]))


(test (allocate mt-store (list (v*s (numV 5) mt-store)) -1) (loc*sto 0 (list (cell 0 (numV 5)))))
(test (allocate mt-store (list (v*s (numV 5) mt-store) (v*s (numV 6) mt-store)
                               (v*s (boolV #t) mt-store)) -1)
      (loc*sto 0 (list (cell 2 (boolV #t))
                       (cell 1 (numV 6))
                       (cell 0 (numV 5)))))
(test (allocate (list (cell 2 (boolV #t))
                      (cell 1 (numV 6))
                      (cell 0 (numV 5))) (list (v*s (numV 7) mt-store)
                                               (v*s (numV 8) mt-store)) -1)
      (loc*sto 3 (list (cell 4 (numV 8))
                       (cell 3 (numV 7))
                       (cell 2 (boolV #t))
                       (cell 1 (numV 6))
                       (cell 0 (numV 5)))))
(test (interp (arrayC 3 (list (numC 5) (numC 6) (numC 7))) mt-env mt-store)
      (v*s (arrayV 3 0) (list (cell 2 (numV 7)) (cell 1 (numV 6)) (cell 0 (numV 5)))))

(test (interp (refC (idC 'p) (numC 2))
             (list (bind 'p 0))
             (list (cell 2 (numV 3)) (cell 1 (numV 2)) (cell 0 (numV 1))))
      (v*s (numV 3) (list (cell 2 (numV 3)) (cell 1 (numV 2)) (cell 0 (numV 1)))))