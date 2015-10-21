#lang plai-typed
(test (top-eval '{{func main {f 6}}
                    {func f x {if {<= 0 x}
                                  {with {z = {+ 9 14}} {y = 98} {+ z y}}
                                  {* {- x {/ x 2}} 2}}}}) 6)