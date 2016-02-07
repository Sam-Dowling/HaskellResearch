import Control.Monad
import Prelude
import System.IO


{-
main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
-}


respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs


main = do   
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents) 

--main = interact respondPalindromes 
