buildDictionary :: [a] -> [b] -> Maybe [(a, b)]

buildDictionary [] [] = Just []
buildDictionary _ [] = Nothing 
buildDictionary [] _ = Nothing
buildDictionary (x:xs) (y:ys) = 
	case buildDictionary xs ys of 
   Nothing -> Nothing
   Just xs -> Just ((x, y) : xs)
	
