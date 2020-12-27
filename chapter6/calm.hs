calm :: [Char] -> [Char]

calm [] = []
calm (x:xs) = if x == '!' then '.' : calm xs
			  else x : calm xs

calmv2 l = map (\x -> if x == '!' then '.' else x) l
