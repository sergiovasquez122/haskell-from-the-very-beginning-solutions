import Text.Read
import System.IO
-- Q1. Write a function to build an IO action which, when run, prints a list of numbers to the screen in the saem format Haskell uses
printListInner :: Show a => [a] -> IO()
printList :: Show a => [a] -> IO()

printListInner [] = do return ()
printListInner (x:y:xs) =  
	do 
	putStr (show x)
        putStr ","
        printListInner (y:xs)
printListInner (x:xs) = 
	do putStr (show x)
           printListInner xs

printList xs = 
	do putStr "["
           printListInner xs
           putStrLn "]"
-- Q2. Write an IO action to read three integers from the user, and return them as a tuple. 
getIntegerMaybe :: IO (Maybe Integer) 
getIntegerMaybe = 
	do 
	line <- getLine
	return (readMaybe line :: Maybe Integer)

getIntegerRobust = 
	do i <- getIntegerMaybe
           case i of 
	      Nothing -> do putStrLn "Not a number, try again"
		     	    getIntegerRobust
	      (Just x) -> return x

getTriple = 
        do	
	x <- getIntegerRobust
        y <- getIntegerRobust
        z <- getIntegerRobust
        return (x, y, z)
