import Control.Concurrent
import Control.Monad
import Text.Printf
import Network
import System.IO

port :: Int
port = 44444

talk :: Handle -> IO ()
talk h = do
   hSetBuffering h LineBuffering
   loop
   where
      loop = do
      line <- hGetLine h
      if line == "end"
         then hPutStrLn h ("Goodbye")
      else do hPutStrLn h (show (2 * (read line :: Integer)))
      loop
               
main = withSocketsDo $ do
   sock <- listenOn (PortNumber (fromIntegral port))
   printf "Listening on port %d\n" port
   forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle) (\_ -> hClose handle)


