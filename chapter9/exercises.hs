-- 1. Rewrite the summary paragraph at the end of this chapter for the three argument function g a b c
-- g = \a -> \b -> \c -> ...
-- 2. Recall the function elem' e l which determines if an element e is contained in a list l. What is its type? What is the type of elem' e? Use partial appliction to write  function elemAll e ls which determines if an element is a member of all the lists in the list of lists ls
elem' :: (Eq a) => a -> [a] -> Bool

elem' _ []  = False
elem' k (x:xs) = k == x || elem' k xs

elemAll :: (Eq a) => a -> [[a]] -> Bool

elemAll = ...
-- 3. Write a function which maps a function over list of list of lists, using the technique described in this chapter.
mapll :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll = map . map . map
