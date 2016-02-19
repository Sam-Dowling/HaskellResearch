data Optional a = Invalid | Valid a 
  deriving (Show)

safeRoot :: Double -> (Optional Double, Bool)
safeRoot x
  | x >= 0    = (Valid (sqrt x), True)
  | otherwise = (Invalid, False)

safeReciprocal :: Double -> (Optional Double, Bool)
safeReciprocal 0   = (Invalid, False)
safeReciprocal num = (Valid (1/num), True)

(>=>) :: (a -> (Optional b,Bool)) -> (b -> (Optional c,Bool)) -> (a -> (Optional c,Bool))
f1 >=> f2 = \x ->
  let fstResult = f1 x
  in case fstResult of
    (_,False)       -> (Invalid, False)
    (Valid value,b) -> let sndResult = f2 value 
                           in (fst sndResult, b && snd sndResult)
    (Invalid,_)     -> (Invalid, False)

safeRootReciprocal :: Double -> (Optional Double, Bool)
safeRootReciprocal = safeReciprocal >=> safeRoot

main :: IO ()
main = putStrLn $ show $ safeRootReciprocal 0

-- safeRootReciprocal 4 = (Valid 0.5, True)
-- safeRootReciprocal 0 = (Invalid, False)


