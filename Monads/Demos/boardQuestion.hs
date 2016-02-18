
import Prelude hiding ((>>=))

data DataM =  Success [MyData] | Failure [MyData] deriving Show

data MyData = Merchant | Financial | Student | None deriving Show

(>>=) :: DataM -> ([MyData] -> DataM) -> DataM
val >>= f = case val of
              Success (x) -> f x
              Failure (x) -> Failure $ None : x

functionA :: [MyData] -> DataM
functionA x = Success $ Merchant : x -- Pass!

functionB :: [MyData] -> DataM
functionB x = Success $ Financial : x -- Pass!

functionC :: [MyData] -> DataM
functionC x = Failure $ None : x -- This function fails!

run = Success [] >>= functionA >>= functionB >>= functionC >>= functionA
