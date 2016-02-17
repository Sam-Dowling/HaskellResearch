{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (mapM_)
import Control.Monad.IO.Class  (liftIO)
import Database.Persist (selectList,entityVal,insertMany)
import Database.Persist.Sqlite (runSqlite,runMigration)
import qualified Database.Persist.TH as TH

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"] [TH.persistLowerCase|
MyRecord
  value Int
  deriving Show
|]

readDB :: IO ()
readDB = runSqlite "test.sqlite" $ do

    records <- selectList [] []
    let values = map (myRecordValue . entityVal) records
    mapM_ (liftIO . putStrLn . show) $ zip values $ tail values

writeDB :: IO ()
writeDB = runSqlite "test.sqlite" $ do

  let n = 100
  runMigration migrateAll
  insertMany $ map MyRecord [1..n]

  return ()
