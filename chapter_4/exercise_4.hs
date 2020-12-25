dropLast :: [a] -> [a]

dropLast [] = []
dropLast [x] = [x]
dropLast (_:xs) = dropLast xs
