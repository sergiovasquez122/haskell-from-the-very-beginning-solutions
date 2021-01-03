import Text.Read
import System.IO

printListInner :: Show a => [a] -> IO()
printList :: Show a => [a] -> IO()

printListInner [] = do return ()
printListInner (x:y:xs) =  
	do 
	putStr (show x)
        putStr ","
        printListInner (y:xs)
printListInner (x:xs) = 
	do putStr (show x)
           printListInner xs

printList xs = 
	do putStr "["
           printListInner xs
           putStrLn "]"
