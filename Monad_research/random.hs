import System.Random

randPairs :: (RandomGen g, Random a) => (a,a) -> g -> [a]
randPairs range gen = randomRs range gen  

seed   = 123456 :: Int
mygen  = mkStdGen seed

coords :: [Int]
coords = take 50 $
    randPairs (1,100) mygen 
