{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Concurrent.Async (concurrently)
import           Control.Monad            (void)
import           Data.Conduit.Network

main :: IO ()
main =
    runTCPClient (clientSettings 4242 "localhost") $ \server ->
        void $ concurrently
            (stdin $$ appSink server)
            (appSource server $$ stdoutC)
