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

module AuthorRepository(
  authorSchema,
  getAllAuthor,
  createAuthor,
  updateAuthor,
  deleteAuthor
) where
 
import Database
import Classes
import Rel8
import Data.Int
import Data.Text
import Text.Read
import Hasql.Session

authorSchema :: TableSchema (Author Name)
authorSchema = TableSchema
  { name = "author"
  , schema = Nothing
  , columns = Author
      { author_id = "author_id"
      , author_name = "author_name"
      , author_url = "author_url"
      }
  }


getAllAuthor :: Query (Author Expr)
getAllAuthor = each authorSchema


createAuthor :: Int64 -> String -> String ->IO()
createAuthor id name url = do
  Right conn <- dbConnection
  let nameConv  = pack name :: Column Result Text
  let urlConv   = readMaybe url :: Column Result (Maybe Text)
  let ins = Insert { into = authorSchema
                   , rows = values [ lit Author { author_id = AuthorId id, author_name = nameConv, author_url = urlConv } ]
                   , onConflict = Abort
                   , returning = NumberOfRowsAffected
                   }
  res <- runStatement () (insert ins) conn
  either printErr (printRes "Records inserted: ") res
  putStrLn "Finished"


updateAuthor :: Int64 -> IO ()
updateAuthor auth_id =  do
  Right conn <- dbConnection
  let upd = Update { target = authorSchema
                   , from = getAllAuthor
                   , set = \_ row -> row { author_url = litExpr $ Just "https://www.google.fr/" }
                   , updateWhere = \_ row -> author_id row ==. litExpr (AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (update upd)) conn
  either printErr (printRes "Records updated: ") res
  putStrLn "Finished"


deleteAuthor :: Int64 -> IO()
deleteAuthor auth_id = do
  Right conn <- dbConnection
  let del = Delete { from = authorSchema
                   , using = getAllAuthor
                   , deleteWhere = \_ row -> author_id row ==. litExpr (Classes.AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (delete del)) conn
  either printErr (printRes "Records deleted: ") res
  putStrLn "Finished"