sqrtMaybe :: (Num a, Ord a) => a -> Maybe a


sqrtMaybe x 
   | x < 0  = Nothing
   | otherwise  = Just (f 1 x)
 	where 
		f a x = if a * a > x then a - 1 else f (a + 1) x
