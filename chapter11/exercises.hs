data Tree a = Br a (Tree a) (Tree a)
		| Lf deriving Show

-- 1. Write a function of type Eq a => a -> Tree a -> Bool to determine if a given element is in a tree.
treeLookup :: Eq a => a -> Tree a -> Bool
treeLookup _ Lf = False
treeLookup k (Br x l r) = k == x || (treeLookup k l) || (treeLookup k r)
-- 2. Write a function which flips a tree left to right such that, if it were draw on paper, it would appear to be a mirror image
mirrorTree :: Tree a -> Tree a

mirrorTree Lf = Lf
mirrorTree (Br x l r) = Br x (mirrorTree r) (mirrorTree l) 
-- 3. Write a function to determine if a two trees have the same shape, irrespective of the actual values of the elements.
sameStructure :: Tree a -> Tree b -> Bool

sameStructure Lf Lf = True
sameStructure _ Lf = False
sameStructure Lf _ = False
sameStructure (Br _ l r) (Br _ l' r') = (sameStructure l l') && (sameStructure r r')
-- 4. Write a function treeOfList which builds a ttree representation of a dictionary froma list representation of a dictionary
treeInsert :: Ord a => Tree (a, b) -> a -> b -> Tree (a, b)

treeInsert Lf k v = Br (k, v) Lf Lf
treeInsert (Br (k', v') l r) k v = 
	if k == k' then Br (k, v) l r else
	if k < k' then Br (k', v') (treeInsert l k v) r else
		Br (k', v') l (treeInsert r k v)

listDictToTreeDict :: Ord a => [(a, b)] -> Tree (a, b) 
listDictToTreeDict [] = Lf
listDictToTreeDict ((k, v):xs) = treeInsert (listDictToTreeDict xs) k v
