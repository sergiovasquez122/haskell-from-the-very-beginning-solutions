import System.Environment
import CopyFiles

main :: IO()

main = 
	do args <- getArgs
           case args of 
	     [infile, outfile] -> do 
                                  CopyFiles.copy infile outfile
             _ ->                 putStrLn "Usage source destination"

