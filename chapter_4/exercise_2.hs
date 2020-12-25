countTrue :: Num p => [Bool] -> p
countTrue [] = 0
countTrue (True:xs) = 1 + countTrue xs
countTrue (False:xs) = countTrue xs
