module Reverse where

import System.IO

reverseHelper f1 f2 = 
	do e <- hIsEOF f1
           if e then return ()
		else do
	             l <- hGetLine f1
                     hPutStrLn f2 (reverse l)
                     reverseHelper f1 f2

reverseLines source destination = 
	do 
	f1 <- openFile source ReadMode
        f2 <- openFile destination WriteMode 
        reverseHelper f1 f2
        hClose f1
        hClose f2
