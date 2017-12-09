(* list tests *)
let _ = print isNil (#3) in
let _ = print isNil (!3) in 
let _ = print (3@(#1)) in 
let _ = print (3@Nil) in 
let listl = 3@4@5@6 in
let _ = print !listl in
let _ = print #listl in
let _ = print !#listl in
let _ = print (3@7@"sdlkfj") in 0
