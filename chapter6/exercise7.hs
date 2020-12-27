all' :: (a -> Bool) -> [a] -> Bool

all' f [] = True
all' f (x:xs) = f x && all' f xs

-- all' (\x -> x `rem` 2 == 0) [1 .. 10]
