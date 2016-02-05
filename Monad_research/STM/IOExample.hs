import Data.Char (toUpper)


shoutInput :: IO String
shoutInput = do
    str <- getLine
    return $ shout str 


--  "Hello" -> "HELLO!"
shout :: String -> String
shout = (map toUpper) . (++ "!")
