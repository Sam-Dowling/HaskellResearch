{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

import Data.Text (Text)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
    share, sqlSettings)    
import Database.Persist.Sql (insert)
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
User
    name     Text
    email    Text
    EmailKey email
    password Text
    isadmin  Bool
    deriving Show
Blog
    title    Text
    url      Text
    author   UserId
    deriving Show
|]


main = runSqlite ":memory:" $ do -- :memory:
    buildDB
    selectUserBlogs "sam@mail.com"
    countBlogs
    updateEmail "sam@mail.com" "sam@mynewemail.net" "alice@mail.org"


selectUserBlogs u = do
    blogs <- select $ from $ \(a, t) -> do
            where_ (a ^. UserEmail ==. val u &&. t ^. BlogAuthor ==. a ^. UserId)
            return (t ^. BlogTitle)
    liftIO $ print blogs


countBlogs = do
    blogs <- select $ from $ \(a, t) -> do
        where_ (t ^. BlogAuthor ==. a ^. UserId)
        groupBy (a ^. UserId)
        let cnt = countRows :: SqlExpr (Value Int)
        orderBy [desc cnt]
        return (a ^. UserEmail, cnt)
    liftIO $ print blogs


updateEmail old new del = do

    update $ \a -> do
        set a [UserEmail =. val new]
        where_ (a ^. UserEmail ==. val old)
    
    delete $ from $ \t ->
        where_ (t ^. UserEmail ==. val del &&. t ^. UserIsadmin ==. val False)
    
    auths <- select $ from $ \a -> return a
    liftIO $ mapM_ (print . userEmail . entityVal) auths


buildDB = do
    runMigrationSilent migrateTables
    sam <-   insert $ User "Sam" "sam@mail.com" "password" True
    alice <- insert $ User "Alice" "alice@mail.org" "pass" False
    bob <-   insert $ User "Bob" "bob@bobmail.ie" "mypass" True
    insert $ Blog "Blog1" "blogurl.com" sam
    insert $ Blog "Blog2" "blogurl.net" sam
    insert $ Blog "Blog3" "blogurl.org" alice
    insert $ Blog "Blog4" "blogurl.ie" bob
