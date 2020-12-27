filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (x:xs) = if f x then x : filter' f xs
			else filter' f xs
