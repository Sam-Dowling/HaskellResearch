import Data.Char (toUpper)
import Control.Applicative

shoutInputDo :: IO String
shoutInputDo = do
    str <- getUserInput
    return $ shout str


shoutInputApplicative :: IO String
shoutInputApplicative = shout <$> getUserInput


getUserInput :: IO String
getUserInput = putChar '>' >> getLine


-- "Hello" -> "HELLO!"
shout :: String -> String
shout = (map toUpper) . (++ "!")
