import Control.Monad
import Control.Concurrent.Thread.Delay
import Control.Concurrent

swap :: MVar a -> MVar a -> IO()
swap v1 v2 = do
        a <- takeMVar v1
        b <- takeMVar v2
        putMVar v1 b
        putMVar v2 a



main :: IO ()
main = do
        v1 <- newMVar ("This is the first String" :: String)
        v2 <- newMVar ("This is the second String" :: String)
        swap v1 v2

        
        forkIO $ do
            text <- getLine
            putMVar v1 text
        
        print =<< takeMVar v1
        print =<< takeMVar v2

        print =<< takeMVar v1
