import Control.Monad.ST
import Data.IORef
import Data.STRef

sumST :: Num a => [a] -> a
sumST xs = runST $ do           -- runST takes out stateful code and makes it pure again.
   n <- newSTRef 0             -- Create an STRef (place in memory to store values)
   forM_ xs $ x -> do         -- For each element of xs ..
      modifySTRef n (+x)
      readSTRef n      

addone :: Num a => a -> a
addone n = n + 1
  
exampleIORef :: IO Int
exampleIORef = do
  counter <- newIORef 0
  modifyIORef counter (+ 1)
  putStrLn "im in ur IO monad so i can do I/O"
  readIORef counter

