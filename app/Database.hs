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
    runStatement,
    printErr
)
 where

import qualified Hasql.Connection
import Hasql.Connection
import Hasql.Statement
import Hasql.Session
import Rel8
import System.Environment (getEnv)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)

connection :: IO String
connection = getEnv "DB_CON"

-- conect to postgresql db
dbConnection :: IO (Either ConnectionError Connection)
dbConnection = do
    connStr <- connection
    Hasql.Connection.acquire ( encodeUtf8 . pack $ connStr)

runStatement :: params -> Statement params result -> Connection -> IO (Either QueryError result)
runStatement params stmnt = run (statement params stmnt)


-- Run query with a text
-- ex: runqry "List of all projects" getAllProjects
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