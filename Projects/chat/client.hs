{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import Network (connectTo, PortID (..))
import System.Environment (getArgs, getProgName)
import System.IO


main :: IO ()
main = do
        host <- getLine
        telnet host


telnet :: String -> IO ()
telnet host = runResourceT $ do
    (releaseSock, hsock) <- allocate (connectTo host $ PortNumber $ fromIntegral 4242) hClose
    liftIO $ mapM_ (`hSetNewlineMode` NewlineMode CRLF CRLF) [ stdin, stdout, hsock ]  --mapM_ (`hSetBuffering` LineBuffering) [ stdin, stdout, hsock ]
    (releaseThread, _) <- allocate (forkIO $ runResourceT $ sourceHandle stdin $$ sinkHandle hsock) killThread
    sourceHandle hsock $$ sinkHandle stdout
    release releaseThread
    release releaseSock
