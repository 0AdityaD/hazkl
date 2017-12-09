(* test 4 by Andy *)

let sss = (lambda x. lambda y. lambda z. (x + y + z + 1)) in
let gvar = 2 + (sss 1 3 5) in 
let x = (8 + 1) in
(gvar * 2)