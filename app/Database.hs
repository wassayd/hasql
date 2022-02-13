{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
 
module Database(
    dbConnection,
    runqry,
    printRes,
    printErr
)
 where

import qualified Hasql.Connection
import Hasql.Connection
import Hasql.Statement
import Hasql.Session
import Rel8

dbConnection :: IO (Either ConnectionError Connection)
dbConnection = Hasql.Connection.acquire "postgresql://postgres:root@127.0.0.1:5432/rel8testing"

runStatement :: params -> Statement params result -> Connection -> IO (Either QueryError result)
runStatement params stmnt = run (statement params stmnt)


runqry :: (Serializable exprs (FromExprs exprs), Show (FromExprs exprs)) => String -> Query exprs -> IO ()
runqry cap qry = do
    Right conn <- dbConnection
    putStrLn cap
    res <- runStatement () (select qry) conn
    either printErr (mapM_ print) res


printErr :: Show a => a -> IO()
printErr = printRes "Error: "

printRes :: Show a => String -> a -> IO()
printRes cap val = putStrLn $ cap <> show val