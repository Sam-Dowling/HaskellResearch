{-# LANGUAGE OverloadedStrings #-}
module Main where

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



data User = User { name :: String, age :: Int } deriving Show

instance QueryResults User where
  convertResults [fa,fb] [va,vb] = User { name = a, age = b }
    where a = convert fa va
          b = convert fb vb
  convertResults fs vs  = convertError fs vs 2

clean :: SqlCommand
clean = sqlCmd_ "drop table if exists users"

create :: SqlCommand
create = sqlCmd_ "create table if not exists users (name text, age int)"

insert :: String -> Int -> SqlCommand
insert name age = sqlCmd "insert into users (name, age) values (?, ?)" (name,age)

select :: SqlQuery [User]
select = sqlQuery_ "select name, age from users"

user :: (String,Int) -> User
user (name,age) = User { name = name, age = age }

demo :: SqlQuery [User]
demo = clean >>> create >>> insert "Alice" 26 >>> insert "Bob" 24 >>> select

main :: IO ()
main = do
  conn  <- connect connectInfo
  users <- demo conn
  _     <- putStrLn $ show users
  return ()
