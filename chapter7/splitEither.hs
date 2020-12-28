splitEither :: (a -> Either b c) -> [a] -> ([b], [c])

splitEither _ []  = ([], [])
splitEither f (x:xs) = 
	let (ls, rs) = splitEither f xs 
	in 
		case f x of 
    	Left l -> (l : ls, rs)
	Right r -> (ls, r : rs)
