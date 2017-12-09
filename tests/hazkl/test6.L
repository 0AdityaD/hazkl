let _ = print (lambda x,y,z. x@y@z 2 3) @ (lambda x.x) in
let _ = print (lambda x,y. x+y 8 0) in
let f = lambda n. if n=0 then 1 else n* (f (n-1)) in
let _ = print (f 4) in
fun fact with n = if n=0 then 1 else n* (fact (n-1)) in
(fact 4)


