doubleList :: Num a => [a] -> [a]

doubleList [] = []
doubleList (x:xs) = (x * x) : doubleList xs
