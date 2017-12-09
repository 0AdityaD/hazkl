(* test functions/expression lists *)
fun cat with list1, list2 =
if isNil list1 then list2 
else !list1@(cat #list1 list2) in
fun reverse with list =
let tail = !list in
if isNil #list then tail
else let head = (reverse #list) in
(cat head tail)
in
(reverse 1@2@3@4@5)
