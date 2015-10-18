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

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)
