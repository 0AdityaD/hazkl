
let H = lambda f, n. (if (n = 0) then 1 else (n * (f (n-1)))) in

 
let Y = (lambda f. (lambda x. (f (x x)) (lambda x. (f (x x))) )) in

if ((Y H) 5) > 100 then 1-1 else 5
