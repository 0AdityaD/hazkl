(* test 1: replacement shadowing *)
let x = 5 in ((let x = 2 in (lambda a. x)) 0)
