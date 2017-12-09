(* Factoral lambda : recursive lambda *)
let f = lambda n. if n=0 then 1 else n* (f (n-1)) in (f 5)
