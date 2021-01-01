-- 1. write the list whose elements are the numbers 1, 2, 4, 8, 16 ...
f :: (Num a) => [a]
f = [2 ^ x | x <- [0 .. ]]

-- 2. Write a function which, given a list, returns the list which consists of infinitely many copies of that list. For example, given the list [1, 2, 3] it should return a list with elements 1, 2, 3, 1, 2, 3, 1, 2 ...
copies :: [a] -> [a]
copies x = if length x /= 0 then x ++ copies x else x
-- 3. Write a list whose elements are the Fibonacci numbers 0, 1, 1, 2, 3, 5, 8 ... whose first two elements are zero and one by definition, and each ensuing element is the sum of the previous two
fib :: (Num a) => a -> a -> [a]
fibInner :: (Num a) => a -> a -> [a]

fib x y = fibInner x y
fibInner x y = (x + y) : fibInner y (x + y)
-- 5. Write the function unleave which, given a list, return two lists, one containing elements at positions 0, 2, 4, ... of the original list, and the other containing elements at postions 1, 3, 5, 7 ...
unleave :: [a] -> ([a], [a])
unleave l = helper l False [] []

helper :: [a] -> Bool -> [a] -> [a] -> ([a], [a])
helper (x:xs) False l1 l2 = helper xs True (x:l1) l2
helper (x:xs) True l1 l2 = helper xs False l1 (x:l2)
helper [] _ l1 l2 = (reverse l1, reverse l2)
