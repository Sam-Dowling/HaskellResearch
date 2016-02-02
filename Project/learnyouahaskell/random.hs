import System.IO
import Data.List

solveRpn :: (Num a, Read a) => String -> a
solveRpn = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction xs numberString = read numberString:xs
          

main = do line <- fmap reverse getLine
          putStrLn $ "you said " ++ line
