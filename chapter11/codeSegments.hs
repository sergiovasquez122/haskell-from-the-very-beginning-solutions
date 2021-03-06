data Tree a = Br a (Tree a) (Tree a)
		| Lf deriving Show

treeSize :: Num a => Tree b -> a

treeSize (Br _ l r) = 1 + treeSize l + treeSize r
treeSize Lf = 0

treeTotal :: Num a => Tree a -> a

treeTotal (Br x l r) = x + treeTotal l + treeTotal r
treeTotal Lf = 0

maxDepth :: (Num b, Ord b) => Tree a -> b

maxDepth Lf = 0
maxDepth (Br _ l r) = 1 + max (maxDepth l) (maxDepth r)

listOfTree :: Tree a -> [a]
listOfTree (Br x l r) = listOfTree l ++ [x] ++ listOfTree r
listOfTree Lf = []

treeMap :: (a -> b) -> Tree a -> Tree b

treeMap _ Lf = Lf
treeMap f (Br x l r) = Br (f x) (treeMap f l) (treeMap f r)

treeLookup :: Ord a => Tree (a, b) -> a -> Maybe b

treeLookup lf _ = Nothing
treeLookup (Br (k', v) l r) k = 
	if k == k' then Just v
	    else if k < k' then treeLookup l k else treeLookup r k

treeInsert :: Ord a => Tree (a, b) -> a -> b -> Tree (a, b)

treeInsert Lf k v = Br (k, v) Lf Lf
treeInsert (Br (k', v') l r) k v = 
	if k == k' then Br (k, v) l r else
	if k < k' then Br (k', v') (treeInsert l k v) r else
		Br (k', v') l (treeInsert r k v)
