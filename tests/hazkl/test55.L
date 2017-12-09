fun for with a, b =
if a = b then Nil
else a@(for a+1 b) in

fun for_each with a, f =
if isNil a then 0
else let _ = (f !a) in
(for_each #a f) in

fun mod with a, b =
if a < b then a
else (mod a-b b) in

fun sum with a =
let r = 0 in
let l = lambda x.let r = r+x in 0 in
(for_each a l) in

fun bitwise_and with a, b =
if (a = 0) | (b = 0) then 0 else
if (mod a 2) = (mod b 2) then
(mod a 2) + 2*(bitwise_and a/2 b/2) else
2*(bitwise_and a/2 b/2) in

let _ = print (bitwise_and 1 1) in
let _ = print (bitwise_and 2 2) in
let _ = print (bitwise_and 4 4) in
let _ = print (bitwise_and 1 2) in
let _ = print (bitwise_and 1 3) in
let _ = print (mod 100 10) in
let _ = print (mod 25 3) in
let arr = (for 0 10) in
let _ = (for_each arr lambda x.print x) in
(sum (for 0 10))