module TextStat where 

import System.IO

type Stats = (Integer, Integer, Integer, Integer)

length' :: Num b => [a] -> b

length' [] = 0
length' (x:xs) = 1 + length' xs

statsFromChannel :: Handle -> Stats -> IO Stats

statsFromChannel fh (lines, characters, words, sentences) = 
	do ended <- hIsEOF fh
           if ended then 
		    return (lines, characters, words, sentences)
                    else 
                      do 
                      line <- hGetLine fh
                      let charCount = length' line
                          wordCount = length' (filter (\x -> x == ' ') line)
			  sentenceCount = length' (filter (\x -> x == '.' || x == '?' || x == '!') line)
                      statsFromChannel fh((lines + 1), (characters + charCount), (words + wordCount), (sentences + sentenceCount))

statsFromFile :: FilePath -> IO Stats

statsFromFile filename = 
	do fh <- openFile filename ReadMode 
           result <- statsFromChannel fh (0, 0, 0, 0)
           hClose fh
           return result
