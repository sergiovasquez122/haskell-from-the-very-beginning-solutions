revInner :: [a] -> [a] -> [a]
reverse' :: [a] -> [a]

revInner a [] = a
revInner a (x:xs)  = revInner (x : a) xs

reverse' l = revInner [] l
