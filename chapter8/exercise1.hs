length' :: (Num a) => [b] -> a
length' [] = 0
length' (x:xs) = 1 + length' xs
