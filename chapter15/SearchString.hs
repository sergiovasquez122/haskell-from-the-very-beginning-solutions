import System.Environment
import System.IO

matched_string [] _ = True
matched_string _ [] = False
matched_string (x:xs) (y:ys) = (x == y) && matches xs ys

matches [] [] = True
matches _ [] = False
matches xs (y:ys) = matched_string xs (y:ys) ||  matches xs ys

retrieveLines fh = 
    do 
    e <- hIsEOF fh
    if e then return []
	 else do
	      x <- hGetLine fh
              xs <- retrieveLines fh
              return (x:xs)

outputList [] = return ()
outputList (x:xs) = do
	            putStrLn x
                    outputList xs

searchFile filename searchString = 
	do 
	fh <- openFile filename ReadMode
        xs <- retrieveLines fh
	let xs' = filter (matches searchString) xs
        outputList xs'
        hClose fh

main :: IO()

main = 
	do args <- getArgs
           case args of 
	     [infile, theString] -> 
		                    do 
			            searchFile infile theString
             _                   ->  putStrLn "Usage: filename string"
