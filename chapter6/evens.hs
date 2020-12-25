evens :: Integral a => [a] -> [Bool]

evens [] = []
evens (x:xs) = (x `rem` 2 == 0) : evens xs
