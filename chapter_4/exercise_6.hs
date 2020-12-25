elem' :: Eq a => a -> [a] -> Bool

elem' a [] = False
elem' a (x:xs) = if x == a then True else elem' a xs

makeSet :: Eq a => [a] -> [a]

makeSet [] = []
makeSet (x:xs) = if elem' x xs then makeSet xs else x : makeSet xs
