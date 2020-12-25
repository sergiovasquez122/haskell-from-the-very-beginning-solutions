oddElements :: [a] -> [a]

oddElements (x:_:xs) = x : oddElements xs
oddElements l = l
