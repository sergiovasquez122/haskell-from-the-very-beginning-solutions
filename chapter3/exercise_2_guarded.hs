sum' :: (Eq a, Num a) => a -> a

sum' x | x == 1     = x
       | otherwise  = x + sum' (x - 1)
