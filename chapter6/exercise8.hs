mapl :: (a -> b) -> [[a]] -> [[b]]

mapl f [] = []
mapl f (x:xs) = (map f x) : (mapl f xs)
