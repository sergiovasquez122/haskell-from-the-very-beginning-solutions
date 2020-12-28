replace :: Eq a => a -> b -> [(a, b)] -> Maybe [(a, b)]

replace k v [] = Nothing
replace k v ((k', v') : xs) = if k == k' then Just ((k, v) : xs) 
					 else let xs' = replace k v xs
	    in 
						case xs' of 
						Nothing -> Nothing
						Just xs' -> Just ((k', v'):xs')
