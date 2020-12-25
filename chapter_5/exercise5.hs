insert :: Ord a => a -> [a] -> [a]

insert a [] = [a]
insert a (x:xs) = if a <= x then a:x:xs
		   else x:insert a xs

sort :: Ord a => [a] -> [a]

sort [] = []
sort (x:xs) = insert x (sort xs)

-- [1, 2] < [2, 3] -> true
-- sort ["one", "two", "three"] -> ["one", "three", "two"]

