module CopyFiles where

import System.IO

copyHelper f1 f2 = 
	do e <- hIsEOF f1
           if e then return ()    
		else do 
		     l <- hGetLine f1 
                     hPutStrLn f2 l
                     copyHelper f1 f2

copy source destination = 
	do 
	f1 <- openFile source ReadMode
        f2 <- openFile destination WriteMode
        copyHelper f1 f2
        hClose f1
        hClose f2
