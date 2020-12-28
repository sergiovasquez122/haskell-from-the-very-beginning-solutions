smallest0 :: (Num a, Ord a) => [a] -> a

smallest0 [] = 0
smallest0 [x] = if x > 0 then x else 0
smallest0 (x:xs) = if x <= 0 then smallest0 xs
			     else let y = smallest0 xs
			     in if y > 0 && y < x then y else x
