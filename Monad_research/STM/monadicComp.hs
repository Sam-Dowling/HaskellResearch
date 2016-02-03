
type Positive = Int

c :: (Positive -> Positive) -> Positive -> Maybe Positive
c f n 
    | n > 0 = Just (f n)
    | otherwise = Nothing

subone :: Positive -> Positive
subone n = n - 1

addone :: Positive -> Positive
addone n = n + 1
