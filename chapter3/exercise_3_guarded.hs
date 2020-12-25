power :: (Eq a, Num a, Num b) => b -> a -> b

power x n | n  == 0     = 1
      | n  == 1     = x
      | otherwise   = x * power x (n-1)
