sign :: (Ord a, Num a, Num b) => a -> b

sign x if x < 0 then -1 else if x == 0 then 0 else 1
