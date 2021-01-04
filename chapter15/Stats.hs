import System.Environment
import Textstat

main :: IO()

main = 
	do args <- getArgs
           case args of 
	     [infile] -> do 
		         (l, c, w, s) <- Textstats.statsFromFile inFile
	                 putStr "Lines: "
                         putStrLn (show l)
                         putStr "Words: " 
                         putStrLn (show w)
                         putStr "Sentences: "
                         putStrLn (show s)
              _ -> putStrLn "Usage: Stats <filename>"
