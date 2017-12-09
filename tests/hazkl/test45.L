fun rev with a, b =
    if isNil a then b else (rev #a !a@b)
in (rev 1@2@3@4@5 Nil)
