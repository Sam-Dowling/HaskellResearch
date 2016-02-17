{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

import Data.Text
import Data.Conduit
import Data.Conduit.List as CL
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
User
    username Text
    password Text
    isadmin  Bool
    deriving Show
Blog
    title    Text
    url      Text
    author   UserId
    deriving Show
|]


main = runSqlite ":memory:" $ do
    buildDB
    selectUser "Sam"
    dumpTable

selectUser u = do
    user <- selectList [UserUsername ==. u][]
    liftIO $ print $ user

buildDB = do
    runMigrationSilent migrateTables
    sam <-   insert $ User "Sam" "password" True
    alice <- insert $ User"Alice" "pass" False
    bob <-   insert $ User "Bob" "mypass" True
    insert $ Blog "Blog1" "blogurl.com" sam
    insert $ Blog "Blog2" "blogurl.net" sam
    insert $ Blog "Blog3" "blogurl.org" alice
    insert $ Blog "Blog4" "blogurl.ie" bob

dumpTable = rawQuery "select * from Blog" [] $$ CL.mapM_ (liftIO . print)
    
