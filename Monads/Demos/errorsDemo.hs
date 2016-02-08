{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Error

div :: Float -> Float -> Float
div x y = x / y

divMaybe :: Float -> Float -> Maybe Float
divMaybe x 0 = Nothing
divMaybe x y = Just (x / y)


divEither :: Float -> Float -> Either String Float
divEither x 0 = Left "Division by zero"
divEither x y = Right (x / y)

divFail :: (Monad m) => Float -> Float -> m Float
divFail x 0 = fail "Division by zero"
divFail x y = return (x / y)

divFailMaybe x y =
    case divFail x y of
        Nothing -> putStrLn "Division by zero"
        Just q  -> putStrLn (show q)

divFailEither x y =
    case divFail x y of
        Left msg -> putStrLn msg
        Right q  -> putStrLn (show q)

-------------------------------------------------------

data CustomError = DivByZero
                 | NumberTooBig
                 | MiscError String

instance Show CustomError where
    show DivByZero = "Division by zero"
    show (MiscError str) = str


divError :: (MonadError CustomError m) => Float -> Float -> m Float
divError _ 0 = throwError DivByZero
divError 1 1 = throwError $ MiscError "Diving 1 by 1 is pointless!"
divError x y = return (x / y)

runDivError :: Float -> Float -> Either CustomError String
runDivError x y =
    catchError (do q <- divError x y
                   return (show q))
               (\err -> return (show err))
