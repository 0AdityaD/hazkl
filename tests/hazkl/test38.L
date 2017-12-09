
(* lambda tests *)
let _ = print ((lambda x. x) 3) in
let _ = print ((lambda x,y. x + 2*y) 2 4) in
let _ = print (lambda x. lambda y. lambda z.x+y 3 2) in
fun f1 with x, y =
x + 5 in
fun f2 with x =
(f1 x 4)
in
let _ = print (f2 3) in 0
