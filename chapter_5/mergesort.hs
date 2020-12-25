merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = 
	if x < y 
    		then x : merge xs (y:ys)
		else y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = 
	let 
	    left = take (length l `div` 2) l
	    right = drop (length l `div` 2) l
 	in 
		merge (mergeSort left) (mergeSort right)

