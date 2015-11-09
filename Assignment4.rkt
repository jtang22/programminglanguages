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
  [setMutC (name : ExprC) (val : ExprC)])

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
    [(empty? env) (error 'lookup (string-append "lookup failed " (to-string for)))]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(test (lookup 's (list (bind 'h 5) (bind 's 6))) 6)
(test/exn (lookup 'z (list (bind 'h 5))) "lookup failed")

;Fetch data from memory location
(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch (string-append "out of bounds " (to-string loc)))]
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

;Create list of params for with function
;Takes a list of s-expressions, parses through left hand params
;Returns a list of symbols
(define (create-func-lam [sl : (listof s-expression)]) : (listof symbol)
  (cond
    [(empty? sl) empty]
    [(s-exp-match? '(ANY = ANY) (first sl))
     (cons (s-exp->symbol (first (s-exp->list (first sl))))
           (create-func-lam (rest sl)))]
    [else (error 'create-func-lam "invalid format")]))

;Create list of args for with call
;Takes a list of s-expressions, parses through right hand assignments
;Returns a list of ExprC
(define (create-func-args [sl : (listof s-expression)]) : (listof ExprC)
  (cond
    [(empty? sl) empty]
    [(s-exp-match? '(ANY = ANY) (first sl))
     (cons (parse (third (s-exp->list (first sl)))) (create-func-args (rest sl)))]
    [else (error 'create-func-args "invalid format")]))

(test/exn (create-func-args (list '{x})) "invalid format")

;Give list of symbols to check for dups
;Takes a list of symbols
;Returns a false if there are dups, else returns true
(define (check-params [params : (listof symbol)]) : boolean
  (cond
    [(empty? params) #t]
    [(equal? #t (member (first params) (rest params))) #f]
    [else (and #t (check-params (rest params)))]))


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
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-number? (first sl))
          (parse (first sl))]
         [(s-exp-boolean? (first sl))
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
         [(s-exp-match? '(func SYMBOL ... ANY) s)
          (let ([params (map s-exp->symbol (reverse (rest (reverse (rest sl)))))])
           (cond
            [(equal? #t (check-params params))
             (lamC params (parse (first (reverse sl))))]
            [else (error 'parse "Duplicate params")]))]
         [(s-exp-match? '(ANY ANY ...) s)
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
(test (parse `true) (boolC #t))
(test (parse `false) (boolC #f))
(test (parse '{eq? #t #f}) (binop 'eq? (boolC #t) (boolC #f)))
(test (parse '{func x y {+ x y}}) (lamC (list 'x 'y) (binop '+ (idC 'x) (idC 'y))))
(test (parse '{with {x = {+ 5 4}} {+ x 1}}) (appC (lamC (list 'x) (binop '+ (idC 'x) (numC 1))) (list (binop '+ (numC 5) (numC 4)))))
(test (parse '1) (numC 1))
(test (parse '{#t}) (boolC #t))
(test (parse '{if {eq? x 5} {x} {- x 5}}) (ifC (binop 'eq? (idC 'x) (numC 5)) (appC (idC 'x) (list)) (binop '- (idC 'x)(numC 5))))
(test (parse '{5}) (numC 5))
(test (parse '{func 9}) (lamC (list) (numC 9)))
(test (parse '{array 3 14 false 5}) (arrayC 4 (list (numC 3) (numC 14) (boolC #f) (numC 5))))
(test (parse '{new-array 3 14}) (arrayC 3 (list (numC 14) (numC 14) (numC 14))))
(test (parse '{p [15] <- {f 6}}) (setArrC (idC 'p) (numC 15) (appC (idC 'f) (list (numC 6)))))
(test (parse '{begin {f 9} p}) (beginC (list (appC (idC 'f) (list (numC 9)))) (idC 'p)))
(test (parse '{l <- 9}) (setMutC (idC 'l) (numC 9)))
(test (parse '{ref p [15]}) (refC (idC 'p) (numC 15)))
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

;Interprets functions for evalution
;Takes an ExprC and an environment
;Returns a value based on evaluated expression
(define (interp [exp : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC exp
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                    [v*s (v-f s-f)
                         (type-case Value v-f
                           [cloV (p b e)(let ([args-list (interp-elems a env s-f)])
                           (type-case Lookups (add-env-and-sto
                                               e (get-store args-list s-f) (cloV-param v-f)
                                               args-list)
                             [env*sto (e-l s-l) (interp (cloV-body v-f) e-l s-l)]))]
                           [else (error 'interp "not a closure")])])]
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
    [lamC (p b) (v*s (cloV p b env) sto)]
    [boolC (b) (cond
                 [(equal? b #t) (v*s (boolV #t) sto)]
                 [else (v*s (boolV #f) sto)])]
    [ifC (c t e) (type-case Result (interp c env sto)
                   [v*s (v-c s-c)
                        (type-case Value v-c
                          [boolV (b) (cond
                                       [(equal? b #t) (interp t env s-c)]
                                       [else (interp e env sto)])]
                          [else (error 'interp "non-boolean value")])])]
    [arrayC (size ele) (type-case Alloc (allocate sto (interp-elems ele env sto) -1)
                       [loc*sto (l s) (v*s (arrayV size l) s)])]
    [refC (name loc) (type-case Result (interp name env sto)
                       [v*s (v-l s-l)
                            (type-case Value v-l
                              [arrayV (size start) (type-case Result (interp loc env s-l)
                                                     [v*s (v-r s-r)
                                                          (v*s (fetch (+ (numV-num v-r) start) s-r) s-r)])]
                              [else (type-case Result (interp loc env s-l)
                                      [v*s (v-r s-r)
                                           (v*s (fetch (numV-num v-r) s-r) s-r)])])])]
    [beginC (exp v) (interp v env (interp-begins exp env sto))]
    [setMutC (n v) (type-case Result (interp v env sto)
                     [v*s (v-r s-r) (v*s v-r (cons (cell (lookup (idC-x n) env) v-r) s-r))])]
    [setArrC (name ndx val) (type-case Result (interp val env sto)
                              [v*s (v-r s-r) (type-case Result (interp ndx env sto)
                                               [v*s (v-n s-n)
                                                    (type-case Value (fetch (lookup (idC-x name) env) s-n)
                                                      [arrayV (s b) (v*s v-r (cons (cell (+ b (numV-num v-n)) v-r) s-n))]
                                                      [else (error 'interp "unbound array")])])])]))

(test (interp (binop '+ (numC 5) (numC 6)) mt-env mt-store) (v*s (numV 11) mt-store))
(test (interp (binop '- (numC 6) (numC 5)) mt-env mt-store) (v*s (numV 1) mt-store ))
(test (interp (binop '* (binop '/ (numC 10) (numC 2)) (numC 2)) mt-env mt-store) (v*s (numV 10) mt-store))
(test (interp (binop '/ (numC 10) (numC 5)) mt-env mt-store) (v*s (numV 2) mt-store))
(test (interp (binop '<= (numC 10) (numC 11)) mt-env mt-store) (v*s (boolV #t) mt-store))
(test (interp (binop '<= (numC 10) (numC 9)) mt-env mt-store) (v*s (boolV #f) mt-store))
(test (interp (binop 'eq? (boolC #t) (boolC #t)) mt-env mt-store) (v*s (boolV #t) mt-store))
(test (interp (binop 'eq? (boolC #f) (boolC #t)) mt-env mt-store) (v*s (boolV #f) mt-store))
(test (interp (binop 'eq? (numC 5) (numC 6)) mt-env mt-store) (v*s (boolV #f) mt-store))
(test (interp (binop 'eq? (numC 5) (numC 5)) mt-env mt-store) (v*s (boolV #t) mt-store))
(test (interp (ifC (boolC #t) (numC 5) (numC 6)) mt-env mt-store) (v*s (numV 5) mt-store))
(test (interp (ifC (boolC #f) (numC 5) (numC 6)) mt-env mt-store) (v*s (numV 6) mt-store))
(test (interp (setMutC (idC 'p) (numC 5))
              (list (bind 'p 0))
              (list (cell 0 (numV 4))))
      (v*s (numV 5) (list (cell 0 (numV 5)) (cell 0 (numV 4)))))
(test (interp (setArrC (idC 'p) (numC 2) (numC 6))
              (list (bind 'p 3))
              (list (cell 3 (arrayV 3 0)) (cell 2 (numV 4)) (cell 1 (numV 3)) (cell 0 (numV 2))))
      (v*s (numV 6) (list (cell 2 (numV 6)) (cell 3 (arrayV 3 0)) (cell 2 (numV 4)) (cell 1 (numV 3)) (cell 0 (numV 2)))))
(test/exn (interp (ifC (numC 5) (numC 5) (numC 5)) mt-env mt-store) "non-boolean value")
(test/exn (interp (appC (numC 5) (list (numC 6))) mt-env mt-store) "not a closure")
(test/exn (interp (setArrC (idC 'p) (numC 2) (numC 6))
              (list (bind 'p 0))
              (list (cell 0 (numV 4)))) "unbound array")

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
   [else (allocate (cons (cell (find-next-base sto -1) (v*s-v(first val))) sto) (rest val) base-loc)]))

;Interps elements of array and threads store through them
(define (interp-elems [elems : (listof ExprC)] [env : Env] [sto : Store]) : (listof Result)
  (cond
    [(empty? elems) empty]
    [else (let ([res (interp (first elems) env sto)])
            (type-case Result res
            [v*s (v-f s-f)
                 (cons res (interp-elems (rest elems) env s-f))]))]))

;Interps expressions of a begin statement
(define (interp-begins [exprs : (listof ExprC)] [env : Env] [sto : Store]) : Store
  (cond
    [(empty? exprs) sto]
    [else (let ([res (interp (first exprs) env sto)])
            (type-case Result res
              [v*s (v-f s-f) (interp-begins (rest exprs) env s-f)]))]))

;Gets latest store from a list of results
(define (get-store [results : (listof Result)] [sto : Store]) : Store
  (cond
    [(empty? results) sto]
    [else (v*s-s (first (reverse results)))]))

;Adds an array to storage
(define (add-array-store [sto-ret : Store] [sto-arr : Store] [array  : Value] [cur-loc : Location]) : Store
    (cond
      [(= (arrayV-size array) cur-loc) sto-ret]
      [else (let ([ndx (+ cur-loc (arrayV-base array))]
                  [next-loc (find-next-base sto-ret -1)])
              (add-array-store (cons (cell next-loc(fetch ndx sto-arr)) sto-ret)
                               sto-arr array (+ 1 cur-loc)))]))

;Adds on to environment and store for an application
(define (add-env-and-sto [env : Env] [sto : Store] [params : (listof symbol)] [args : (listof Result)]) : Lookups
  (cond
    [(empty? params)
     (cond
       [(empty? args) (env*sto env sto)]
       [else (error 'add-env-and-sto "wrong arity")])]
    [(= (length params) (length args))
     (type-case Result (first args)
       [v*s (v-f s-f)
            (let ([where (find-next-base sto -1)])
              (type-case Value v-f
                [arrayV (size b)
                        (add-env-and-sto
                         (extend-env (bind (first params) where) env)
                         (add-array-store sto s-f v-f 0) (rest params) (rest args))]
                [else (add-env-and-sto (extend-env (bind (first params) where) env)
                               (override-store (cell where v-f) sto)
                               (rest params) (rest args))]))])]
    [else (error 'add-env-and-sto "wrong arity")]))

(test (add-env-and-sto mt-env mt-store (list 'x 'y) (list (v*s (numV 5) mt-store) (v*s (numV 6) mt-store)))
      (env*sto (list (bind 'y 1) (bind 'x 0)) (list (cell 1 (numV 6)) (cell 0 (numV 5)))))
(test (add-env-and-sto mt-env (list (cell 0 (numV 5))) (list 'x) (list (v*s (arrayV 2 0) (list (cell 1 (numV 3)) (cell 0 (numV 4))))))
      (env*sto (list (bind 'x 1)) (list (cell 2 (numV 3)) (cell 1 (numV 4)) (cell 0 (numV 5)))))
(test/exn (add-env-and-sto (list) (list (cell 0 (numV 5))) (list) (list (v*s (numV 5) mt-store))) "wrong arity")
(test/exn (add-env-and-sto (list) (list) (list 'x) (list (v*s (numV 6) mt-store)(v*s (numV 5) mt-store))) "wrong arity")

;Takes a Value
;Returns the value as a string
(define (serialize [val : Result]) : string
  (type-case Value (v*s-v val)
    [numV (n) (to-string n)]
    [cloV (p b e) "#<procedure>"]
    [boolV (b) (cond
                 [(equal? #t b) "true"]
                 [else "false"])]
    [arrayV (s b) "#<array>"]))

(test (serialize (v*s (numV 5) mt-store)) "5")
(test (serialize (v*s (boolV #t) mt-store)) "true")
(test (serialize (v*s (boolV #f) mt-store)) "false")
(test (serialize (v*s (cloV (list 'h) (numC 5) mt-env) mt-store)) "#<procedure>")
(test (serialize (v*s (arrayV 1 0) mt-store)) "#<array>")

;Evaluates an s-expression, represents as a string
(define (top-eval [s : s-expression]) : string
  (serialize (interp (parse s) mt-env mt-store)))

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



(test (top-eval (quote ((func seven (seven))
                  ((func minus
                         (func (minus (+ 3 10) (* 2 3))))
                   (func x y (+ x (* -1 y))))))) "7")

(test (interp (beginC (list (numC 5) (arrayC 2 (list (numC 2) (numC 3)))) (numC 6)) mt-env mt-store)
      (v*s (numV 6) (list (cell 1 (numV 3)) (cell 0 (numV 2)))))
(test (top-eval '((func
             empty
             ((func
               cons
               ((func
                 empty?
                 ((func
                   first
                   ((func
                     rest
                     ((func
                       Y
                       ((func
                         length
                         ((func addup (addup (cons 3 (cons 17 empty))))
                          (Y
                           (func
                            addup
                            (func l (if (empty? l) 0 (+ (first l) (addup (rest l)))))))))
                        (Y
                         (func
                          length
                          (func l (if (empty? l) 0 (+ 1 (length (rest l)))))))))
                      ((func x (func y (y (func z (((x x) y) z)))))
                       (func x (func y (y (func z (((x x) y) z))))))))
                    (func l (l false))))
                  (func l (l true))))
                (func l (eq? l empty))))
              (func a b (func select (if select a b)))))
            13)) "20")

(test (top-eval '(with (p = 1472) (begin (p <- (array 1 2 3 4 5)) (ref p [2])))) "3")
(parse (quote (with (f = (new-array 5 false)) (begin (f (0) <- 19) (f ((+ 0 1)) <- 20) (f (0) <- 87) (+ (* 100 (ref f (0))) (ref f (1)))))))
(top-eval (quote (with (f = (new-array 5 false)) (begin (f (0) <- 19) (f ((+ 0 1)) <- 20) (f (0) <- 87) (+ (* 100 (ref f (0))) (ref f (1)))))))
(interp (appC
 (lamC
  (list 'f)
  (beginC
   (list (setArrC (idC 'f) (numC 0) (numC 19)) (setArrC (idC 'f) (appC (binop '+ (numC 0) (numC 1)) (list)) (numC 20)) (setArrC (idC 'f) (numC 0) (numC 87)))
   (binop '+ (binop '* (numC 100) (refC (idC 'f) (numC 0))) (refC (idC 'f) (numC 1)))))
 (list (arrayC 5 (list (boolC #f) (boolC #f) (boolC #f) (boolC #f) (boolC #f))))) mt-env mt-store)
(top-eval (quote (with (f = (new-array 5 false)) (begin (f (0) <- 19) (f ((+ 0 1)) <- 20) (f (0) <- 87) (+ (* 100 (ref f (0))) (ref f (1)))))))
(top-eval (quote (with (a = 9) (b = (array 3 false true 19)) (d = 999) (with (c = (func (begin (d <- b) (b (3) <- 333) (+ (ref d (3)) a)))) (c)))))
(top-eval (quote (with (a = 0) (with
                                (a! = (func expected (if (eq? a expected) (a <- (+ 1 a)) (/ 1 0))))
                                (begin (+ (a! 0) (a! 1))
                                       (if (begin (a! 2) true) (a! 3) (/ 1 0))
                                       (new-array (begin (a! 4) 34) (begin (a! 5) false))
                                       ((begin (a! 6) (new-array 3 false)) ((begin (a! 7) 2)) <- (begin (a! 8) 98723))
                                       (with (p = 9) (p <- (a! 9)))
                                       ((begin (a! 10) (func x y (begin (a! 13) (+ x y))))
                                        (begin (a! 11) 3) (begin (a! 12) 4)) 14)))))
(top-eval (quote (with (halt = 1)
                       (memory = (new-array 1000 0))
                       (pc = 0)
                       (with (go = (with (go = 3735928559)
                                         (begin (go <- (func (with
                                                              (opcode = (ref memory (pc)))
                                                              (if (eq? opcode 0) (begin (pc <- (+ 1 pc)) (go)) (if (eq? opcode 1) pc (/ 1 0)))))) go)))
                             (begin (memory (453) <- halt) (go))))))
