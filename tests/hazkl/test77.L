
fun getLast with l =
    let debugg = (print "Start") in
    let debugg = (print l) in
    let tail = (#l) in
    let debugg = (print l) in
    if isNil tail then
        l
    else
        (getLast tail)
in

(getLast 1@2@3@4)
