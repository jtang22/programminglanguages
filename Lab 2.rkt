#lang plai-typed

;function that combines a list of strings
(define (rev-str-app [strList : (listof string)]) : string
  (cond
    [(empty? strList) ""]
    [else (string-append (first strList) (rev-str-app(rest strList)))]))

(test (rev-str-app empty) "")
(test (rev-str-app(cons "hello" empty)) "hello")
(test (rev-str-app(cons "hello" (cons "hi" empty))) "hellohi")

;define type for processors
(define-type Processors
  [Intel(num : number)]
  [AMD(num : number)]
  [ARM(num : number)])

(define intel1 (Intel 1))
(define intel2 (Intel 2))
(define amd1 (AMD 1))
(define amd2 (AMD 2))
(define arm1 (ARM 1))
(define arm2 (ARM 2))

;Consumes a list of processors and returns a list with only Intel processors
(define (onlyIntels [procList : (listof Processors)]) : (listof Processors)
  (cond
    [(empty? procList) empty]
    [else (type-case Processors (first procList)
            [Intel(n) (cons (first procList) (onlyIntels(rest procList)))]
            [AMD(n) (onlyIntels(rest procList))]
            [ARM(n) (onlyIntels(rest procList))])]))
(test (onlyIntels empty) empty)
(test (onlyIntels (list intel1)) (list intel1))
(test (onlyIntels (list intel1 amd1 arm1 intel2)) (list intel1 intel2))

;Consumes a list of processors and returns a list with only AMD processors
(define (onlyAMDs [procList : (listof Processors)]) : (listof Processors)
  (cond
    [(empty? procList) empty]
    [else (type-case Processors (first procList)
            [AMD(n) (cons (first procList) (onlyAMDs(rest procList)))]
            [Intel(n) (onlyAMDs(rest procList))]
            [ARM(n) (onlyAMDs(rest procList))])]))
(test (onlyAMDs empty) empty)
(test (onlyAMDs (list amd1)) (list amd1))
(test (onlyAMDs (list amd1 intel1 arm1 amd2)) (list amd1 amd2))

;Consumes a list of processsors and returns a list with only ARM processors
(define (onlyARMs [procList : (listof Processors)]) : (listof Processors)
  (cond
    [(empty? procList) empty]
    [else (type-case Processors (first procList)
            [ARM(n) (cons (first procList) (onlyARMs(rest procList)))]
            [Intel(n) (onlyARMs(rest procList))]
            [AMD(n) (onlyARMs(rest procList))])]))
(test (onlyARMs empty) empty)
(test (onlyARMs (list arm1)) (list arm1))
(test (onlyARMs (list arm1 intel1 amd1 arm2)) (list arm1 arm2))

;Consumes a list of processors and a type of processor, return list with specified type only
(define (onlyThese [procList : (listof Processors)] [f : (Processors -> boolean)]) : (listof Processors)
  (cond
    [(empty? procList) empty]
    [(f (first procList)) (cons (first procList) (onlyThese (rest procList) f))]
    [else (onlyThese (rest procList) f)]))

(test (onlyThese empty Intel?) empty)
(test (onlyThese (list arm1 intel1 amd1) AMD?) (list amd1))
(test (onlyThese (list arm1 intel1 amd1) Intel?) (list intel1))
(test (onlyThese (list arm1 intel1 amd1) ARM?) (list arm1))

;Consumes two lists, appends the second one to the first
(define (my-append [list1 : (listof string)] [list2 : (listof string)]) : (listof string)
  (cond
    [(empty? list1) list2]
    [else (cons (first list1) (my-append (rest list1) list2))]))

(test (my-append empty empty) empty)
(test (my-append (list "hello" "hi") (list "hey")) (list "hello" "hi" "hey"))
;Consumes a list and a number and returns all elements after index n
(define (my-drop [numList : (listof number)] [n : number]) : (listof number)
  (cond
    [(empty? numList) empty]
    [(= n 0) numList]
    [else (my-drop (rest numList) (- n 1))]))
(test (my-drop empty 0) empty)
(test (my-drop (list 1 2 3) 2) (list 3))
(test (my-drop (list 1 2 3 4 5) 3) (list 4 5))