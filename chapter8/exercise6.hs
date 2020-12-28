lookup' :: Eq a => a -> [(a, b)] -> Maybe b

lookup' k' [] = Nothing
lookup' k' ((k, v):xs) = if k' == k then Just v else lookup' k' xs

keyExists :: (Eq a, Eq b) => a -> [(a, b)] -> Bool

keyExists k d = lookup' k d /= Nothing

buildDictionaryFromPairs :: (Eq a, Eq b) => [(a, b)] -> [(a, b)]
helper :: (Eq a) => [a] -> [(a, b)] -> [(a, b)]


helper _ [] = []
helper xs ((k, v):ys) = 
	if elem k xs
    	then helper xs ys
	else (k , v) : helper (k:xs) ys

buildDictionaryFromPairs xs =
	helper [] xs

union dict1 dict2 = buildDictionaryFromPairs (dict1 ++ dict2)
