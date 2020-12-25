is_sorted :: (Ord a) => [a] -> Bool

is_sorted [] = True
is_sorted [x] = True
is_sorted (x:y:xs) = x <= y && is_sorted (y:xs)
