isEven :: Integral a => a -> Bool
evens :: Integral a => [a] -> Bool

isEven x = x `rem` 2 == 0
evens x = map' isEven x
