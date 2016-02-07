module MySQL where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.QueryResults
import GHC.Int

type SqlQuery a = Connection -> IO a
type SqlCommand = SqlQuery Int64

sqlQuery :: (QueryParams q, QueryResults r) => Query -> q -> Connection -> IO [r]
sqlQuery q vs conn = query conn q vs

sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

sqlCmd :: QueryParams q => Query -> q -> Connection -> IO Int64
sqlCmd q vs conn = execute conn q vs

sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q

(>>>) :: SqlQuery a -> SqlQuery b -> SqlQuery b
(>>>) q1 q2 conn = do
  q1 conn
  q2 conn
