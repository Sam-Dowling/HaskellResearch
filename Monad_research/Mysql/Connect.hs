{-# LANGUAGE OverloadedStrings #-}

-- https://hackage.haskell.org/package/mysql-simple-0.2.2.5/docs/Database-MySQL-Simple.html
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


data User = User { username :: String, password :: String, isAdmin :: Int } deriving Show

instance QueryResults User where
  convertResults [fa,fb,fc] [va,vb,vc] = User { username = a, password = b, isAdmin = c }
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
  convertResults fs vs  = convertError fs vs 2

user :: (String,String,Int) -> User
user (username,password,isAdmin) = User { username = username, password = password, isAdmin = isAdmin }

{-
users | CREATE TABLE `users` (
  `username` varchar(100) NOT NULL,
  `password` varchar(40) NOT NULL,
  `is_admin` tinyint(1) NOT NULL,
  PRIMARY KEY (`username`)
) 
-}

clean :: SqlCommand
clean = sqlCmd_ "drop table if exists users"

create :: SqlCommand
create = sqlCmd_ "create table if not exists users (username varchar(100) not null,password varchar(40) not null, is_admin tinyint(1) not null, primary key (username))"

insert :: String -> String -> Int -> SqlCommand
insert username password isAdmin = sqlCmd "insert into users (username,password,is_admin) values (?, ?, ?)" (username,password,isAdmin)

selectUser :: String -> SqlQuery [User]
selectUser username = sqlQuery "select password from users where username = ?" [username]

selectAll :: SqlQuery [User]
selectAll = sqlQuery_ "select * from users"

demo :: SqlQuery [User]
demo = selectUser "Bob"

main :: IO ()
main = do
  conn  <- connect connectInfo
  us <- insert "sam" "pass" 1 conn
  _     <- putStrLn $ show us
  return ()


