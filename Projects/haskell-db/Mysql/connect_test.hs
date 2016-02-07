{-# LANGUAGE OverloadedStrings #-}
module Main where



import Control.Applicative
import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar
import Happstack.Server
import MySQL
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Control.Monad.Reader

connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "localhost",
                            connectPort = 3306,
                            connectUser = "root",
                        connectPassword = "",
                        connectDatabase = "haskell",
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }
                             

badConnectInfo :: ConnectInfo
badConnectInfo = ConnectInfo { connectHost = "localhost",
                            connectPort = 3306,
                            connectUser = "root",
                        connectPassword = "",
                        connectDatabase = "Does_Not_Exist",
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }
 

                             
data User = User { username :: String, password :: String, isAdmin :: Int } deriving (Show, Eq)  
  
instance QueryResults User where
  convertResults [fa,fb,fc] [va,vb,vc] = User { username = a, password = b, isAdmin = c }
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
  convertResults fs vs  = convertError fs vs 2


{-
printBool :: String
printBool = do
        x <- selectUser "bob"
        if x then
           "Found"
        else
           "Nope" 
-}

select :: String -> SqlQuery [User]
select x = sqlQuery "select * from users where username = ?" [x]

selectUser :: String -> IO Bool
selectUser a = connect connectInfo >>= select a >>= (\x -> return $ 0 < length x)
  
