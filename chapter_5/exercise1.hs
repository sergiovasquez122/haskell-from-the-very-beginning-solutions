merge :: (Ord a) => [a] -> [a] -> [a]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = if x < y 
			 then x : merge xs (y:ys)
			 else y : merge (x:xs) ys

merge_sort :: (Ord a) => [a] -> [a]

merge_sort [] = []
merge_sort [x] = [x]
merge_sort l = 
	let mid = (length l `div` 2)
		in 
	let left = take mid l
	    right = drop mid l
	in 
	    merge (merge_sort left) (merge_sort right)

