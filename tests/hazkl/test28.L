(*Test 5 - Recursion*)
let f = lambda n. if n = 0 then 1 else n* (f (n-1)) in

(f 4)