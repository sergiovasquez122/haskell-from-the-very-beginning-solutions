import Data.Char
import Data.String
-- 1. Use a function from the Data.Char module to write a function which replaces all upper case characters in a given string with lower case ones.
lowerString :: String -> String
lowerString x = map toLower x
-- 2. Use the functions dropWhile and takeWhile to isolate the section of a list which contain positive numbers. For example, isolate [-6, -4, -1, 0, 1, 2, 4, 5, 3, 2, -1, -9] should yield the list [1, 2, 4, 5, 3, 2]
isolate xs = takeWhile (\x-> x > 0) (dropWhile (\x->x<=0) xs)
-- 3. The word otherwise for guard equations is not really part of the Haskell Language, but is defined in the Standard Prelude. Find or deduce its definition.
-- :type otherwise
-- otherwise :: Bool
-- otherwise == True
-- True
-- 4. Use functions from Data.String to write a program which reverses the order of the words in its input string.
reverseString :: String -> String
reverseString = unwords . reverse . words
-- 5. The functions ord and chr from the Data.Char module convert between numbers and characters. Use these functions to write a function toLower which converts an upper case letter to lower case, leaving any other characters alone. This is a home-made version of the functions you used to solve Question 1 above.
toLower' :: Char -> Char
toLower' x = let 
             ord_value = ord x
             ord_value_of_A = ord 'A'
             ord_value_of_a = ord 'a'
             in
	     if x >= 'A' && x <= 'Z'
	     then chr (ord_value - ord_value_of_A + ord_value_of_a)
             else x
