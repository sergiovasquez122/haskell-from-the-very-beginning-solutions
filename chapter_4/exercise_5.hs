elem' :: Eq a => a -> [a] -> Bool

elem' a [] = False
elem' a (x:xs) = if x == a then True else elem' a xs
