(*
*)

fun insert with l, val =
    if (isNil l) then val else
    if (#val < (#(!l))) then val@l
    else !l@(insert #l val)
in

fun insertion_sort with sorted, l =
    if (isNil l) then sorted else
    (insertion_sort (insert sorted@(Nil@0) !l) #l)
in

fun list_sum with l =
    if (isNil l) then 0 else (!l + (list_sum #l))

in

fun input with _ =
    let _ = print "Input value (between 0 and 100, inclusive):" in
    let val = readInt in
    if ((val >= 0) & (val <= 100)) then val else (input _)
in
    
fun inputToList with num_assignments =
    if num_assignments <= 0 then Nil else
    let weights = (input _) @ weights in
    (*let _ = print weights in*)
    if (num_assignments <= 1) then weights else
    (inputToList num_assignments - 1)
in

fun sum_assignments with weights = (* for a given student, returns grade*weight *)
    if (isNil weights) then 0 else
    (!weights * (input _)) + (sum_assignments #weights)
in

fun input_student with weights =
    let _ = print "Input student name:" in
    let name = readString in
    let _ = print "Input assignment grades:" in
    let grade = (sum_assignments weights)/100 in
    name@grade
in

fun input_students with weights, insert_list =
    let s = (input_student weights) in
    let insert_list = (insert insert_list s) in
    (*let _ = print insert_list in*)
    let _ = print "Add another student? (y/n)" in
    let response = readString in
    if (response = "y") then (input_students weights insert_list)
    else insert_list
in

fun print_students with student_list =
    if (isNil !!student_list) then 0
    else let _ = (print_students #student_list) in 
    let _ = print !!student_list + ":" in
    print #(!student_list)

in

let _ = print "Number of assignments: " in
let x = readInt in
let weights = Nil in
let weights = (inputToList x) in
(* let _ = print "weights: " in
   let _ = print weights in
   let _ = print "sum: " in *)
let weights_sum = (list_sum weights) in
if (weights_sum <> 100) then print "Weights do not sum to 100. Aborting." else
let _ = print "Weights sum to 100" in
let insert_list = (Nil@101)@(Nil@101) in
let student_grades = (input_students weights insert_list) in
(print_students student_grades)

(* (insertion_sort Nil (("asdf2"@2)@("asdf1"@1))) *)
(*(insert (Nil@0)@("asdf2"@2) "asdf1"@1) *)

(*(#(!(#((Nil @ 0) @ ("asdf2" @ 2)))))*)
