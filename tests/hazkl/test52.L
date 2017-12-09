let x = 6 in
let _ = if x then 
	let x = 5 in
	let _ = if x then 
		let x = 4 in 
		print x else 0 in
	print x
	else 0 in
print x