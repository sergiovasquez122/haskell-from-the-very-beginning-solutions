import Text.Read
import System.IO

printDictEntry :: Show a => (a, String) -> IO()

printDictEntry (k, v) = 
	do putStrLn (show k)
    	   putStrLn v

printDict :: Show a => [(a, String)] -> IO()

printDict [] = return ()
printDict (x:xs) = 
	do printDictEntry x
    	   printDict xs

getInteger :: IO Integer

getInteger = 
	do line <- getLine
    	   return (read line :: Integer)

readDict :: IO [(Integer, String)]

readDict = do i <-  getInteger
	      if i == 0 then return []
		 	else do name <- getLine
	    			dict <- readDict
				return ((i, name) : dict)

getIntegerMaybe :: IO (Maybe Integer)
readDictRobust :: IO [(Integer, String)]

getIntegerMaybe = 
	do 
	line <- getLine
    	return (readMaybe line :: Maybe Integer)

readDictRobust =
	do i <- getIntegerMaybe
    	   case i of 
	  	Just 0 -> return []
		Just x -> 
			do 
			name <- getLine
      			dict <- readDictRobust
			return ((x, name) : dict)
		Nothing ->
			do 
			putStrLn "Not a number. Try again."
      			x <- readDictRobust
			return x

entryToHandle :: Show a => Handle -> (a, String) -> IO()
dictionaryToHandle :: Show a => Handle -> [(a, String)] -> IO()
dictionaryToFile :: Show a => FilePath -> [(a, String)] -> IO()

entryToHandle fh (k, v) = 
	do 
	hPutStrLn fh (show k)	
	hPutStrLn fh v

dictionaryToHandle fh [] = return ()
dictionaryToHandle fh (x:xs) = 
		do 
		entryToHandle fh x
		dictionaryToHandle fh xs

dictionaryToFile filename dict = 
	do 
	fh <- openFile filename WriteMode
	dictionaryToHandle fh dict
	hClose fh

entryOfHandle :: Handle -> IO (Maybe (Integer, String))
dictionaryOfHandle :: Handle -> IO (Maybe [(Integer, String)])
dictionaryOfFile :: FilePath -> IO (Maybe [(Integer, String)])

entryOfHandle fh =
	do 
	k <- hGetLine fh
	v <- hGetLine fh
	case readMaybe k :: Maybe Integer of
   		Nothing -> return Nothing
		Just k' -> return (Just (k', v))

dictionaryOfHandle fh = 
	do ended <- hIsEOF fh
	   if ended then return (Just []) 
                    else
                    do 
                    x <- entryOfHandle fh
                    case x of  
		     Nothing -> return Nothing
		     Just x' ->
                       do xs <- dictionaryOfHandle fh 
		          case xs of 
	                    Nothing -> return Nothing
                            Just xs' -> return (Just (x' : xs'))

dictionaryOfFile fileName = 
  do fh <- openFile fileName ReadMode
     dict <- dictionaryOfHandle fh
     hClose fh
     return dict

handleStatistics :: (Show a, Num a) => Handle -> a -> IO()
fileStatistics :: FilePath -> IO()

handleStatistics fh lines = 
	do 
        ended <- hIsEOF fh
    	if ended then 
		 do putStr "There were "
                    putStr (show lines)
                    putStrLn " lines."
        else 
          do 
          line <- hGetLine fh
          handleStatistics fh (lines + 1)

fileStatistics filename = 
	do 
        fh <- openFile filename ReadMode
        handleStatistics fh 0
        hClose fh

data Tree a = Br a (Tree a) (Tree a) | Lf

treeLookup :: Ord a => Tree (a, b) -> a -> Maybe b

treeLookup lf _ = Nothing
treeLookup (Br (k', v) l r) k = if k == k' then Just v else if k < k' then treeLookup l k else treeLookup r k

treeInsert :: Ord a => Tree (a, b) -> a -> b -> Tree (a, b)

treeInsert Lf k v = Br (k, v) Lf Lf
treeInsert (Br (k', v') l r) k v = 
	if k == k' then Br (k, v) l r else
        if k < k' then Br (k', v') (treeInsert l k v) r else Br (k', v') l (treeInsert r k v)

updateHistogram :: (Ord a, Num b) => Tree (a, b) -> [a] -> Tree (a, b)

updateHistogram tr [] = tr
updateHistogram tr (x:xs) = 
    case (lookup tr x) of 
      Nothing -> updateHistogram (treeInsert tr x 1) xs
      Just v -> updateHistogram (treeInsert tr x (v + 1)) xs

printHistogramList :: (Show a, Show b) => [(a, b)] -> IO()
printHistogram :: (Show a, Show b, Ord a, Ord b) => Tree (a, b) -> IO()

printHistogramList [] = return ()
printHistogramList ((k, v) : xs) = 
    do
    putStr "For character "
    putStr (show k)
    putStr " the count is "
    putStr (show v)
    putStrLn "."
    printHistogramList xs

printHistogram tree = printHistogramList (mergeSort (listOf Tree) tree)

handleStatisticsv2 :: (Show a, Show b, Show c, Show d, Num a, Num b, Num c, Num d) => Handle -> a -> b -> c -> d -> Tree (Char, Integer) -> IO()
fileStatisticsv2 :: FilePath -> IO()

handleStatisticsv2 fh lines characters words sentences histogram = 
     do 
     ended <- hIsEOF fh
     if ended then
	      do
              putStr "There were "
              putStr (show lines)
              putStr " lines, making up "
              putStr (show characters)
              putStr " characters with "
              putStr (show words)
              putStr " words in "
              putStr (show sentences)
              putStrLn " sentences."
              printHistogram histogram
     else
       do 
       line <- hGetLine fh
       let charCount = length line
           wordCount = length (filter (\x -> x == ' ') line)
           sentenceCount = length (filter (\x -> x == '.' || x == '!' || x == '?') line)
           histogram = updateHistogram histogram line
       handleStatisticsv2
         fh (lines + 1) (characters + charCount) (words + wordCount) (sentences + sentenceCount) histogram

fileStatisticsv2 fileName = 
    do
    fh <- openFile fileName ReadMode
    handleStatisticsv2 fh 0 0 0 0 Lf
    hClose fh
