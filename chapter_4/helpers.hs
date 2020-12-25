take' :: (Eq a, Num a) => a -> [b] -> [b]
drop' :: (Eq a, Num a) => a -> [b] -> [b]

take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

drop' 0 l = l
drop' n (x:xs) = drop' (n - 1) xs
