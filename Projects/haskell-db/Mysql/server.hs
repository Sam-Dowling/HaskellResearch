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
                             
data User = User { username :: String, password :: String, isAdmin :: Int } deriving Show

instance QueryResults User where
  convertResults [fa,fb,fc] [va,vb,vc] = User { username = a, password = b, isAdmin = c }
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
  convertResults fs vs  = convertError fs vs 2

user :: (String,String,Int) -> User
user (username,password,isAdmin) = User { username = username, password = password, isAdmin = isAdmin }


main :: IO ()
main = simpleHTTP nullConf router


router :: ServerPart String
router = msum
   [ dir "maybe" $ maybeDemo
 --   ,dir "IO" $ ioDemo
    ,maybeDemo
   ]
  
      


maybeDemo :: ServerPart String
maybeDemo = do 
         q <- optional $ lookText "q"
         ok $ show (q)


select :: String -> SqlQuery [User]
select x = sqlQuery "select * from users where username = ?" [x]

IODemo:: String -> IO Bool
IODemo a = connect connectInfo >>= select a >>= (\x -> return $ 0 < length x)
 
