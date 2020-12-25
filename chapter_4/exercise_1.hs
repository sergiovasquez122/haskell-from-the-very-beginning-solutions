evenElements :: [a] -> [a]

evenElements (_:x:xs) = x : evenElements xs
evenElements l = l
