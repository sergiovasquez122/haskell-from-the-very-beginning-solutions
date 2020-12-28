safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeDiv :: Integral a => a -> a -> Maybe a

safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeTake :: (Num a, Ord a) => a -> [b] -> Maybe [b]
safeDrop :: (Num a, Ord a) => a -> [b] -> Maybe [b]

length' :: (Num a) => [b] -> a

length' [] = 0
length' (x:xs) = 1 + length' xs

take' :: (Eq a, Num a) => a -> [b] -> [b]
drop' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

drop' 0 l = l
drop' n  (_:xs) = drop' (n - 1) xs

safeTake n l = if n >= 0 && n <= length' l
   	then Just (take' n l) 
	else Nothing

safeDrop n l = if n >= 0 && n <= length' l
		  then Just (drop' n l)
		  else Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]

mapMaybe _ []= []
mapMaybe f (x : xs) = 
	case f x of 
   		Nothing -> mapMaybe f xs
		Just r -> r : mapMaybe f xs
