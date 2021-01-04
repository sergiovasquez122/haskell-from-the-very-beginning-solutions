module SortFromFile where

import System.IO
import Data.List

readIntoList fh = 
	do e <- hIsEOF fh
           if e then return []
		else do 
                     x <- hGetLine fh
		     xs <- readIntoList fh
                     return (x:xs)

copyListIntoFile fh [] = return ()
copyListIntoFile fh (x:xs) = do 
	                     hPutStrLn fh x
                             copyListIntoFile fh xs

sortFile source destination = 
	do 
	f1 <- openFile source ReadMode
        f2 <- openFile destination WriteMode
        xs <- readIntoList f1
        copyListIntoFile f2 (map show (sort (map (read :: String -> Integer) xs)))
        hClose f1
        hClose f2
