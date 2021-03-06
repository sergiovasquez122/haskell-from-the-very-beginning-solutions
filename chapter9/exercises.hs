-- 1. Rewrite the summary paragraph at the end of this chapter for the three argument function g a b c
-- g = \a -> \b -> \c -> ...
-- 2. Recall the function elem' e l which determines if an element e is contained in a list l. What is its type? What is the type of elem' e? Use partial appliction to write  function elemAll e ls which determines if an element is a member of all the lists in the list of lists ls
elem' :: (Eq a) => a -> [a] -> Bool

elem' _ []  = False
elem' k (x:xs) = k == x || elem' k xs

elemAll :: (Eq a) => a -> [[a]] -> Bool
elemAll e = not . (elem' False) . (map (elem' e))
-- 3. Write a function which maps a function over list of list of lists, using the technique described in this chapter.
mapll :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll = map . map . map
-- 4. Write a function truncateList which takes an integer and a list of lists, and returns a list of lists, each of which has been truncated to the given length. If a list is shorter than the given length, it is unchanged. Make use of partial applications.
-- truncateList :: (Ord a, Num a) => a -> [b] -> [b]
-- truncateLists :: (Ord a, Num a) => a -> [[b]] -> [[b]]
take' :: (Ord a, Num a) => a -> [b] -> [b]
length' :: (Num a) => [b] -> a
map' :: (a->b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs

length' [] = 0
length' (x:xs) = 1 + length' xs

take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

truncateList n l = if length' l >= n then (take' n l) else l
truncateLists l = (map' truncateList) l 
-- 5. Write a function which takes a list of lists of numbers and return the list composed of all the first elements of the lists. If a list is empty, a given number should be used in place of its first element.
firstElt :: a -> [a] -> a
firstElts :: a -> [[a]] -> [a]
firstElt n [] = n
firstElt n (x:_) = x
firstElts n = map' (firstElt n)
-- 6. Use an operator section to write a function which puts a given number onto the front of all the lists in  list of lists of numbers
putFrontl :: (Num a) => a -> [[a]] -> [[a]]

putFrontl n ls = map' (n :) ls
