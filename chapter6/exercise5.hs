insert :: Ord a => (a -> a -> Bool) -> a -> [a] -> [a]

insert f a [] = [a]
insert f a (x:xs) = if f a x then (a:x:xs)
			     else x : insert f a xs

sort :: Ord a => (a -> a -> Bool) -> [a] -> [a]

sort f [] = []
sort f (x:xs) = insert f x (sort f xs)
