{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude
import Rel8
import Data.Text
import GHC.Generics
import Data.Int
import Hasql.Connection
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction)
import Control.Monad.IO.Class
import qualified Control.Exception as Servant
import Hasql.Pool
import Control.Monad.Reader
import Hasql.Session

newtype AuthorId = AuthorId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show)

data Author f = Author
  { authorId :: Column f AuthorId
  , name     :: Column f Text
  , url      :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

data Project f = Project
  { projectAuthorId :: Column f AuthorId
  , projectName     :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Author f)
deriving stock instance f ~ Result => Show (Project f)

authorSchema :: TableSchema (Author Name)
authorSchema = TableSchema
  { name = "author"
  , schema = Nothing
  , columns = Author
      { authorId = "author_id"
      , name = "name"
      , url = "url"
      }
  }

projectSchema :: TableSchema (Project Name)
projectSchema = TableSchema
  { name = "project"
  , schema = Nothing
  , columns = Project
      { projectAuthorId = "author_id"
      , projectName = "name"
      }
  }


getAllProject :: Query (Project Expr)
getAllProject = each projectSchema

getAllAuthor :: Query (Author Expr)
getAllAuthor = each authorSchema

runQueries :: Connection -> IO()
runQueries conn = do
  runqry "Authors:" getAllAuthor conn
  putStrLn "Finished"

runAllQueries :: Connection -> IO()
runAllQueries conn = do
  runqry "Projects:" getAllProject conn
  runqry "Authors:" getAllAuthor conn
  putStrLn "Finished"

runqry :: (Serializable exprs (FromExprs exprs), Show (FromExprs exprs)) => String -> Query exprs -> Connection -> IO ()
runqry cap qry conn = do
  putStrLn cap
  res <- runStatement () (select qry) conn
  either printErr (mapM_ print) res

runStatement :: params -> Statement params result -> Connection -> IO (Either QueryError result)
runStatement params stmnt = run (statement params stmnt)

runInsert :: Connection -> IO()
runInsert conn = do
  let ins = Insert { into = authorSchema
                   , rows = values [ lit Author { authorId = AuthorId 4, name = "Scott Sedgwick", url = Nothing } ]
                   , onConflict = Abort
                   , returning = NumberOfRowsAffected
                   }
  res <- runStatement () (insert ins) conn
  either printErr (printRes "Records inserted: ") res
  putStrLn "Finished"

runUpdate :: Connection -> IO()
runUpdate conn = do
  let upd = Update { target = authorSchema
                   , from = getAllAuthor
                   , set = \_ row -> row { url = litExpr $ Just "https://scott.sedgwick.name" }
                   , updateWhere = \_ row -> authorId row ==. litExpr (AuthorId 4)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (update upd)) conn
  either printErr (printRes "Records updated: ") res
  putStrLn "Finished"

runDelete :: Connection -> IO()
runDelete conn = do
  let del = Delete { from = authorSchema
                   , using = getAllAuthor
                   , deleteWhere = \_ row -> authorId row ==. litExpr (AuthorId 4)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (delete del)) conn
  either printErr (printRes "Records deleted: ") res
  putStrLn "Finished"

printErr :: Show a => a -> IO()
printErr = printRes "Error: "

printRes :: Show a => String -> a -> IO()
printRes cap val = putStrLn $ cap <> show val

connection :: IO (Either ConnectionError Connection)
connection = Hasql.Connection.acquire "postgresql://postgres:root@127.0.0.1:5432/rel8testing"

main :: IO ()
main = do
  conn <- connection
  case conn of
    Left err -> print err 
    Right c  -> do
      putStrLn "Original DB state"
      runQueries c