append :: [a] -> [a] -> [a]

append [] ys = ys
append (x : ys) ys = x : append xs ys
