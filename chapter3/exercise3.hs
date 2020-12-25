-- 3. Use pattern matching to write a function which, given two numbers x and n, computes x n .
power :: (Eq a, Num a, Num b) => b -> a -> b

power x 0 = 1
power x 1 = x
power x n = x * power x (n - 1)

