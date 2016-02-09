import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (replicateM)
import Control.Concurrent.Chan (newChan, writeChan, readChan)

sleepMs n = threadDelay (n * 1000)

getUserInput :: IO String
getUserInput = putChar '>' >> getLine

mvarWait = do
    result <- newEmptyMVar
    
    forkIO (sleepMs 10 >> getUserInput >>= putMVar result)

    putStrLn "Waiting..."
    value <- takeMVar result
    putStrLn ("The answer is: " ++ value)
    
mvarCounter = do
    counter <- newMVar 0

    let increment = do
            count <- takeMVar counter
            putMVar counter $! count + 1
        incrementer = do
            replicateM 1000 increment
            return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 100
    count <- takeMVar counter
    print count
    
channels = do
    messages <- newChan
    writeChan messages "message1"
    writeChan messages "message2"

    -- Read a message from the channel, then output it.
    msg <- readChan messages
    putStrLn msg
    -- Do the same thing again, but more concisely.
    putStrLn =<< readChan messages
