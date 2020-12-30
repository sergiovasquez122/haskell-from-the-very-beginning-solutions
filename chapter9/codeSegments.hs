add :: Num a => a -> a -> a

add x y = x + y

mapl :: (a->b) -> [[a]] -> [[b]]

mapl f l = map (map f) l

x = 5 + 5