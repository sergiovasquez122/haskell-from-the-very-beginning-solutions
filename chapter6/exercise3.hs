clipList :: (Num a, Ord a) => [a] -> [a]

clipList l = map (\x -> if x < 1 then 1 else if x > 10 then 10 else x) l
