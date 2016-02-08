test p (Right t1) (Right t2) | p t1 && p t2 = Right t1
                             | otherwise = Left "nope"


