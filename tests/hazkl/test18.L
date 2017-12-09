(* `cat` defined in Section 2.4 of L Reference Manual *)
fun cat with l1, l2 =
if (isNil l1) then l2 else
!l1@(cat #l1 l2)
in
fun rmval with l, val, finished =
if (isNil l) then Nil else
if ((!l <> val) | finished) then !l @ (rmval #l val finished)
    else (rmval #l val 1)
in
fun findmin with l, min = (* call with min=101 to start *)
    if (isNil l) then min else
    if ((min = 101) | (!l < min)) then (findmin #l !l)
else (findmin #l min)
in
fun selection_sort with sortedList, l =
(* let _ = print l in *)
if (isNil l) then sortedList else
let min = (findmin l 101) in
(* let _ = print "asdf" in
   let _ = print l in *)
(*(selection_sort (cat sortedList (findmin l 101)) (rmval l (findmin l 101) 0))*)
(selection_sort sortedList@(findmin l 101) (rmval l (findmin l 101) 0))
in
(selection_sort Nil 10@9@8)
(* (rmval 3@2@1 1 0) *)
(* (findmin 3@2 101) *)