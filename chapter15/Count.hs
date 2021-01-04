import System.Environment
import CharacterCounter

main :: IO()

main = 
	do args <- getArgs
           case args of 
	     [infile] -> 
		     do putStr "Count: "
	                count <- CharacterCounter.characterCount infile 
                        putStrLn (show count)
             _ ->       putStrLn "Usage: CharacterCounter <filename>"
