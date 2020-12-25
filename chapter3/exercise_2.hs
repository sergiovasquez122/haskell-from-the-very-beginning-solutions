-- 2. Use pattern matching to write a recursive function sumMatch which, given a positive integer n, returns the sum of all the integers from 1 to n.

sum' :: (Ord a, Num a) => a -> a

sum' 1 = 1
sum' n = n + sum'(n - 1)
