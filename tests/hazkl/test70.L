(*Ishani Sahu and Kalpana Vaidya - ishani.sahu7@gmail.com, kalpana.c.vaidya@gmail.com *)
 
 fun sumWeights with list, sum =
     if isNil list then sum
     else
     let head = !list in
     let tail = #list in
     (head + (sumWeights tail sum)) in
 
 let numAssignments = 2 in
 let weights = (50 @ 50) in
 let sumOfWeights = (sumWeights weights 0) in
     print sumOfWeights