
data MyData = Merchant | Financial | Student | None deriving Show


functionA (Right x) = Right $ Merchant : x
functionA (Left x) = Left $ None : x

functionB (Right x) = Right $ Financial : x
functionB (Left x) = Left $ None : x

logfunc = putStrLn . show


goodData = Right [Student]
badData = Left [Student]

run = logfunc . functionB . functionA $ goodData
