import System.Environment
import Reverse

main :: IO()

main = 
	do args <- getArgs
           case args of 
	     [infile, outfile] -> do
		         	  Reverse.reverseLines infile outfile 
             _ ->                 putStrLn "Usage: sourcefile outputfile"
