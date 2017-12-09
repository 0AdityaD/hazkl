fun f with x =
	if (x) then "a"@3@"b"@NIL
	else (lambda x. lambda x. x + x 1@2@3 "hi")

in (f 13)@(f 0)