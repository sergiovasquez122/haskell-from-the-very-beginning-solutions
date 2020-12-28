mapMaybe :: (a -> Maybe b) -> b -> [a] -> [b]

mapMaybe _ _ [] = []
mapMaybe f y (x:xs) = 
	case f x of 
  		Nothing -> y : rs
		Just r -> r : rs
	where
		rs = mapMaybe f y xs
