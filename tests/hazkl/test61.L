
fun fib with x, y, n = 
    if n = 0 then
        x
    else if n = 1 then
        y
    else
        (fib y (x + y) (n - 1))
in 
(fib 0 1 32)
