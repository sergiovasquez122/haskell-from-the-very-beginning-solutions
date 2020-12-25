insert :: (Ord a) => a -> [a] -> [a]

insert a [] = [a]
insert a (x:xs) = if a >= x 
		   then a:x:xs
		   else x:(insert a xs)

sort :: (Ord a) => [a] -> [a]

sort [] = []
sort (x:xs) = insert x (sort xs)
