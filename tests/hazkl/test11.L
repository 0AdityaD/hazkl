(* fun fact with n = 
	if n = 0 then 1 else
	n * (fact (n-1))
in

(fact 5)
*)

let factorial = 
	lambda n. if n <= 0 then 1 else n* (factorial (n-1)) 
in 
(factorial 5)


(* (lambda f.(lambda x.(f(x x)) lambda x.(f(x x))) (lambda factorial, num.(if num <= 0 then 1 else num* (factorial (num-1)))) 5) *)


(*
(lambda f. (lambda x. (f(x x)) lambda x.(f(x x))) (lambda a. (lambda n. if n <= 0 then 1 else n* (a (n-1)))) 5)
*)