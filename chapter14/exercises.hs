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
-- Q3. In our readDict IO action,we waited for the user to type 0 to indicate no more data. This is clumsy. Implement a new readDcit function with a nicer system.
readDict = 
	do putStrLn "How many entries to read?"
           x <- getIntegerMaybe
           case x of 
	     Nothing -> do 
		        putStrLn "Not a number, try again"
                        readDict
             (Just y) -> if y < 0 then 
				  do putStrLn "Not a positive number, try again"
	                             readDict
                                  else 
				     readDictNumber y

readDictNumber n = 
	if n == 0 then return []
	          else
                     do
                     x <- getIntegerRobust
                     name <- getLine
                     remainder <- readDictNumber (n - 1)
                     return ((x, name) : remainder)
-- Q4 Write a function which, given a number x, returns an IO action which, when run, prints the x-times table to a given filename
printFromList fh [] = return ()
printFromList fh (x:xs) = do
	                  hPutStr fh (show x)
                          hPutStr fh "\t"
                          printFromList fh xs

listOfLists fh [] n = return ()
listOfLists fh (x:xs) n = do 
			  printFromList fh (map (x * ) [1 .. n])
                          hPutStrLn fh ""
                          listOfLists fh xs n

timesTable filename x = 
	do fh <- openFile filename WriteMode
	   listOfLists fh [1 .. x] x
           hClose fh
-- Q5 Write a function to count the number of lines in a given file.
readLine fh lineCount = 
	do 
	ended <- hIsEOF fh
        if ended then 
		do 
		putStrLn (show lineCount)
        else 
          do  
	  line <- hGetLine fh
	  readLine fh (lineCount + 1)

numberOfLines filename = 
	do 
	fh <- openFile filename ReadMode
        readLine fh 0
	hClose fh
