(*Test 3 - Ambiguous Expression*)
let e = lambda z. lambda y. y+z in 
let x = 2 in
let x = 3 in
x+x