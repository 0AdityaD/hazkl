fun cat with l1, l2 = if isNil l1 then l2 else !l1@(cat #l1 l2) in
fun reverse_cat with l1, l2 = if isNil l2 then l1 else (cat (reverse_cat !l2 #l2) l1) in 
fun start with list = (cat !list #list) in (start 1@2@3@4@5@6)