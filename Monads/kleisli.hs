
safeRoot :: Double -> (Maybe Double, Bool)
safeRoot x
  | x >= 0    = (Just (sqrt x), True)
  | otherwise = (Nothing, False)

safeReciprocal :: Double -> (Maybe Double, Bool)
safeReciprocal 0   = (Nothing, False)
safeReciprocal num = (Just (1/num), True)

(>=>) :: (a -> (Maybe b,Bool)) -> (b -> (Maybe c,Bool)) -> (a -> (Maybe c,Bool))
f1 >=> f2 = \x ->
  let fstResult = f1 x
  in case fstResult of
    (Just value,b) -> let sndResult = f2 value 
                           in (fst sndResult, b && snd sndResult)
    (Nothing,_)     -> (Nothing, False)

safeRootReciprocal :: Double -> (Maybe Double, Bool)
safeRootReciprocal = safeReciprocal >=> safeRoot

main :: IO ()
main = print $ safeRootReciprocal 6

-- safeRootReciprocal 4      = (Just 0.5, True)
-- id $ safeRootReciprocal 4 = (Just 0.5, True)
-- safeRootReciprocal 0      = (Nothing, False)


