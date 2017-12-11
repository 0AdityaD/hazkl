let factorial = lambda x.
    if x = 0
    then
        1
    else
        x*(factorial x-1)
in
let _ = print (factorial 5) in
fun factorial with x =
    if x = 0
    then
        1
    else
        x*(factorial x-1)
in
let _ = print (factorial 5) in
let _ = print (lambda g. ((lambda h.(g(h h)))(lambda h.(g(h h)))) lambda f. lambda x. if x = 0 then 1 else x*(f x-1) 5) in
let Y = lambda g. ((lambda h.(g(h h)))(lambda h.(g(h h)))) in
let G = lambda f. lambda x. if x = 0 then 1 else x*(f x-1) in
let _ = print (Y G 5) in 0
(*
let Y = lambda g. let _ = print g in ((lambda h.g(h h))(lambda h.g(h h))) in
let G = lambda f. lambda x.
    let r = if x = 0 then 1 else x*(f x-1)
    in
    let _ = print r
    in
    r
in
(Y G 3)
*)