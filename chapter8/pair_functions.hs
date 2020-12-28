fst' :: (a, b) -> a
snd' :: (a, b) -> b

fst' (x, _) = x
snd' (_, y) = y

lookup' :: Eq a => a -> [(a, b)] -> Maybe b

lookup' k' [] = Nothing
lookup' k' ((k, v):xs) = if k' == k then Just v else lookup' k' xs

add :: Eq a => a -> b -> [(a, b)] -> [(a, b)]

add k v [] = [(k, v)]
add k v ((k', v') : xs) = if k == k' then (k, v) : xs
				     else (k', v') : add k v xs

remove :: Eq a => a -> [(a, b)] -> [(a, b)]

remove _ [] = []
remove k' ((k, v) : xs)  = if k' == k then xs
				      else (k, v) : remove k' xs

keyExists :: (Eq a, Eq b) => a -> [(a, b)] -> Bool

keyExists k d = lookup' k d /= Nothing
