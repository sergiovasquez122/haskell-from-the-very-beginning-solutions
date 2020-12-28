decomposeDictionary :: [(a, b)] -> ([a], [b])

decomposeDictionary [] = ([], [])
decomposeDictionary ((k, v) : xs) = 
	let (ls, rs) = decomposeDictionary xs
		in 
			(k:ls, v:rs)
