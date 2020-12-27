clip :: (Num a, Ord a) => [a] -> [a]

clip [] = []
clip (x:xs) = if x < 1 then 1 : clip xs
		       else if x > 10 
			then 10 : clip xs
			else x : clip xs
