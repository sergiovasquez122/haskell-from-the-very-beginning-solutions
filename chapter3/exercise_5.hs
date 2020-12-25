kind :: Num a => Char -> a

kind c | c >= 'a' && c <= 'z' = 0
       | c >= 'A' && c <= 'Z' = 1
       | otherwise            = 2