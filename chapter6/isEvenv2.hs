evens :: Integral a => [a] -> Bool

evens l = map' (\x -> x `rem` 2 == 0) l
