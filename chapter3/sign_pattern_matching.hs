sign :: (Ord a, Num a, Num b) => a -> b

sign x | x < 0      = -1
       | x == 0     = 0
       | otherwise  = 1
