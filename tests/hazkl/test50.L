fun f with x =
	if (x) then "a"@3@"b"@NIL
	else (f 0)

in (f 13)
