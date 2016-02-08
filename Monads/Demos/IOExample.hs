import Data.Char (toUpper)
import System.IO
import Control.Applicative
import Data.IORef

-- instance Applicative Maybe where
--     pure                  = Just
--     (Just f) <*> (Just x) = Just (f x)
--     _        <*> _        = Nothing

--      (+3)  $       9 =      12
--      (+3) <$> Just 9 = Just 12
-- fmap (+3)     Just 9 = Just 12
-- Just (+3) <*> Just 9 = Just 12
-- Pure (+3) <*> Just 9 = Just 12


shoutInputDo :: IO String
shoutInputDo = do
    str <- getUserInput
    return $ shout str



shoutInputApplicative :: IO String
shoutInputApplicative = shout <$> getUserInput


-- return :: Monad m => a -> m a
shoutReturn :: String
shoutReturn = return "Hello" >>= shout


exampleIORef :: IO String
exampleIORef = do
    greeting <- newIORef "Hello"
    modifyIORef greeting shout
    readIORef greeting

-- fmap f x = pure f <*> x
--      (+) <$> Just 10 <*> Just 5 = Just 15
-- Just (+) <*> Just 10 <*> Just 5 = Just 15

readText :: IO Int 
readText = sum . map (\x -> read x :: Int) . words <$> readFile "Nums.txt" 


-- "Hello" -> "HELLO!"
shout :: String -> String
shout = (map toUpper) . (++ "!")

getUserInput :: IO String
getUserInput = putChar '>' >> getLine


