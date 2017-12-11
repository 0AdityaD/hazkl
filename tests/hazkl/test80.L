
#(((lambda x . x) (lambda x . x) (0))@((lambda f . ((lambda x . (f (x x))) (lambda x . (f (x x))))
lambda f . lambda n . (if n = 0 then 1 else n * (f (n - 1)))) (5 + (print 0)))@
(
fun f with x =
    if x < 1 then
        1
    else
        (f (x - 1)) * x
in
(f (5 + print 0)) 
))
