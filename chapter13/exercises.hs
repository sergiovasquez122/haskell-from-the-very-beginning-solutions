-- 1. write the list whose elements are the numbers 1, 2, 4, 8, 16 ...
f :: (Num a) => [a]
f = [2 ^ x | x <- [0 .. ]]

-- 2. Write a function which, given a list, returns the list which consists of infinitely many copies of that list. For example, given the list [1, 2, 3] it should return a list with elements 1, 2, 3, 1, 2, 3, 1, 2 ...
copies :: [a] -> [a]
copies x = if length x /= 0 then x ++ copies x else x
-- 3. Write a list whose elements are the Fibonacci numbers 0, 1, 1, 2, 3, 5, 8 ... whose first two elements are zero and one by definition, and each ensuing element is the sum of the previous two
