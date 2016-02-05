
type Positive = Int -- Positive > 0

check :: (Positive -> Positive) -> Positive -> Maybe Positive
check f n 
    | f n > 0 = Just (f n)
    | otherwise = Nothing

subOne :: Positive -> Positive
subOne = (subtract 1)

safeSubOne :: Positive -> Maybe Positive
safeSubOne = check subOne

{-
    instance Monad Maybe where  
        return x = Just x  
        Nothing >>= f = Nothing  
        Just x >>= f  = f x  
        fail _ = Nothing  
-}

type Writer a = (a, String)

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s1 ++ s2)

return :: a -> Writer a
return x = (x, "")

writeLog :: Maybe Positive -> Writer (Maybe Positive)
writeLog s = ((s >>= safeSubOne), (drop 5(show s)) ++ " - 1 = " ++ (drop 5(show(s >>= safeSubOne))) ++ ", ")

process :: Maybe Positive -> Writer (Maybe Positive)
process = (>=>) writeLog writeLog
