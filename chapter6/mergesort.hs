merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeSort :: (a -> a -> Bool) -> [a] -> [a]

merge _ [] l = l
merge _ l [] = l
merge f (x:xs) (y:ys) = if f x y
			   then x : merge f xs (y:ys)
			   else y : merge f (x:xs) ys
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort f xs = 
	let mid = length xs `div` 2
	in 
	let left = take mid xs
	    right = drop mid xs
	in 
	   merge f (mergeSort f left) (mergeSort f right)
