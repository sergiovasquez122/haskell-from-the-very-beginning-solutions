f :: (Num a) => a -> [a]
g :: (Num a, Enum a) => a -> [a]

f x = [1, x]
g x = [1 .. x]
