import System.Environment
import SortFromFile

main :: IO()

main = 
	do args <- getArgs
           case args of 
	     [infile, outfile] -> 
		                  do 
				  SortFromFile.sortFile infile outfile
             _                 -> putStrLn "Usage: source destination"
