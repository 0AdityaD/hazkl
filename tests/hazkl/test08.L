let _ = print "hello" in 
let w = "world" in 
fun gt with x, y = if x > y then let x=2 in (lambda y. x+y 4) else let x=1 in (lambda y.x 5)
in (gt 1 2)
