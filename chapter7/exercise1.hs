smallest :: (Num a, Ord a) => [a] -> Maybe a

smallest [] = Nothing
smallest [x] = if x <= 0 then Nothing else Just x
smallest (x:xs) = 
	let y = smallest xs
  in 
	case y of 
   		Nothing -> smallest [x]
		Just y -> if x > 0 && x < y then Just x else Just y

