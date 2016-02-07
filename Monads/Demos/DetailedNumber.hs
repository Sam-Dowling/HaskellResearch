import Control.Monad.Writer

data Sign = Positive | Negative deriving (Show,Eq)
type Number = Int
data DetailedNumber = DetailedNumber {
                    sign :: Sign,
                    number :: Number
                    } deriving Show

createNumber :: Number -> DetailedNumber
createNumber n
    | n > 0 = DetailedNumber {sign = Positive, number = n}
    | otherwise = DetailedNumber {sign = Negative, number = abs n}


getNumber :: DetailedNumber -> Number
getNumber (DetailedNumber {sign = s, number = n})
    | s == Positive = n
    | s == Negative = (-n)

takeOne :: DetailedNumber -> DetailedNumber
takeOne n = createNumber . (subtract 1) $ getNumber n

countDown :: DetailedNumber -> Writer [String] DetailedNumber
countDown n
    | getNumber n == (-5) = do
         tell [show $ getNumber n]
         return n
    | otherwise = do
         tell[show $ getNumber n]
         countDown $ takeOne n

main = return $ runWriter (countDown $ createNumber 3)
