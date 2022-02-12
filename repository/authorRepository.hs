module Main where
 
main = undefined

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
  Right conn <- connection
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
  Right conn <- connection
  let upd = Update { target = authorSchema
                   , from = getAllAuthor
                   , set = \_ row -> row { author_url = litExpr $ Just "https://www.google.fr/" }
                   , updateWhere = \_ row -> author_id row ==. litExpr (AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (update upd)) conn
  either printErr (printRes "Records updated: ") res
  putStrLn "Finished"