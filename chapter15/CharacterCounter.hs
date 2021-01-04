module CharacterCounter where

import System.IO

helper fh count = 
	do ended <- hIsEOF fh
           if ended then 
		    return count
                    else
                    do 
                    line <- hGetLine fh
                    helper fh (count + (length line) + 1)

characterCount filename = 
    do
    fh <- openFile filename ReadMode
    result <- helper fh 0
    hClose fh
    return result
