import System.Random

genRand :: (RandomGen g, Random a) => (a,a) -> g -> [a]
genRand range gen = randomRs range gen  

seed   = 123456 :: Int
mygen  = mkStdGen seed

numbers :: [Int]
numbers = take 10 $
    genRand (1,100) mygen
    
    
    
genRandomIO :: IO Int
genRandomIO = randomRIO (1,100)
