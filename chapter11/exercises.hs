data Tree a = Br a (Tree a) (Tree a)
		| Lf deriving Show

-- 1. Write a function of type Eq a => a -> Tree a -> Bool to determine if a given element is in a tree.
treeLookup :: Eq a => a -> Tree a -> Bool
treeLookup _ Lf = False
treeLookup k (Br x l r) = k == x || (treeLookup k l) || (treeLookup k r)
