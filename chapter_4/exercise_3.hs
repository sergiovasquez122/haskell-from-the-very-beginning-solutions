palindrome :: [a] -> [a]

palindrome [] = []
palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]

revInner :: [a] -> [a] -> [a]
reverse' :: [a] -> [a]

revInner a [] = a
revInner a (x:xs)  = revInner (x : a) xs

reverse' l = revInner [] l

equalArray :: (Eq a) => [a] -> [a] -> Bool

equalArray [] [] = True
equalArray [] [x] = False
equalArray [x] [] = False
equalArray (x:xs) (y:ys) = x == y && equalArray xs ys


isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome []  = True
isPalindrome xs = equalArray xs (reverse' xs)
