sumElts :: Num a => [a] -> a

sumElts [] = 0
sumElts (x:xs) = x + sumElts xs
