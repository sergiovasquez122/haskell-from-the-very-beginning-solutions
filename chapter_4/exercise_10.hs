length' :: Num b => [a] -> b

length' [] = 0
length' (x:xs) = 1 + length' xs

countTrue :: (Num a) => [Bool] -> a

countTrue xs =  length' [x | x <- xs, x == True]
