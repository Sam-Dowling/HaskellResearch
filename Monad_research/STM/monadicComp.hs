

type PositiveNumber = Int

addone :: PositiveNumber -> Maybe PositiveNumber
addone n
    | n > 0 = Just (n + 1)
    | otherwise = Nothing
