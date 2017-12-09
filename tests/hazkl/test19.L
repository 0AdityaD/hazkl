(* Reverse list : two functions *)
fun cat with list1, list2 =
    if isNil list1 then list2 else !list1@(cat #list1 list2) in
fun reverse with list =
    if isNil #list then list else (cat (reverse #list) !list) in
(reverse 1@2@3)
