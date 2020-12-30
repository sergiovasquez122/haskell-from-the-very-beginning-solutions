-- 1. Rewrite the summary paragraph at the end of this chapter for the three argument function g a b c
-- g = \a -> \b -> \c -> ...
-- 3. Write a function which maps a function over list of list of lists, using the technique described in this chapter.
mapll :: (a -> b) -> [[[a]]] -> [[[b]]]

mapll = map . map . map
