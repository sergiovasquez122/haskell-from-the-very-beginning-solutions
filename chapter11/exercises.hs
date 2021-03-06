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
-- 5. Write a function to combine two dictionary represented as trees into one. In the case of clashing keys prefer the value from the first dictionary.
mergeTrees :: (Ord a) => Tree (a, b) -> Tree (a, b) -> Tree (a, b)

listOfTree :: Tree a -> [a]
listOfTree (Br x l r) = listOfTree l ++ [x] ++ listOfTree r
listOfTree Lf = []

mergeTrees t t' = listDictToTreeDict (listOfTree t ++ listOfTree t')
-- 6. Can you define a type for trees which, instead of branching exactly two ways each time, can zero or more ways, possibly different at each branch? Write simple functions like our treeSize, treeTotal, and treemap for your new type of tree.
data NaryTree a = Branch a [NaryTree a] deriving Show

treeSize :: (Num b) => NaryTree a -> b
treeSize (Branch _ l) = 1 + (sum (map treeSize l))

treeTotal :: (Num b) => NaryTree b -> b
treeTotal (Branch x l) = x + (sum (map treeTotal l))

treeMap :: (a -> b) -> NaryTree a -> NaryTree b

treeMap f (Branch x l) = Branch (f x) (map (treeMap f) l)
