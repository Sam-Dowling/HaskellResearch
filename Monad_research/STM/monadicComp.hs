
type Positive = Int -- Positive > 0

check :: (Positive -> Positive) -> Positive -> Maybe Positive
check f n 
    | f n > 0 = Just (f n)
    | otherwise = Nothing

subOne :: Positive -> Positive
subOne = (subtract 1)

safeSubOne :: Positive -> Maybe Positive
safeSubOne = check subone
