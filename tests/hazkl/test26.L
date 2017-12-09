fun foo with a = let r = (a 5) in let _ = print r in r in
fun bar with foo = let r = (foo 10) in let _ = print r in r in
let _ = print foo in
let _ = print bar in
(bar (foo lambda x,y.x+y))