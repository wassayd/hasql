module ProjectRepository where

main = undefined


projectSchema :: TableSchema (Project Name)
projectSchema = TableSchema
  { name = "project"
  , schema = Nothing
  , columns = Project
      { projectAuthorId = "author_id"
      , projectName = "project_name"
      }
  }


getAllProject :: Query (Project Expr)
getAllProject = each projectSchema


createProject :: Int64 -> String -> IO ()
createProject authorId prjName = do 
  Right conn <- connection
  let nameConv  = pack prjName :: Column Result Text
  let ins = Insert { into = projectSchema
                   , rows = values [ lit Project { projectAuthorId = AuthorId authorId, projectName = nameConv  } ]
                   , onConflict = Abort
                   , returning = NumberOfRowsAffected
                   }
  res <- runStatement () (insert ins) conn
  either printErr (printRes "Records inserted: ") res
  putStrLn "Finished"


deleteProject :: Int64 -> IO()
deleteProject auth_id = do
  Right conn <- connection
  let del = Delete { from = authorSchema
                   , using = getAllAuthor
                   , deleteWhere = \_ row -> author_id row ==. litExpr (AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (delete del)) conn
  either printErr (printRes "Records deleted: ") res
  putStrLn "Finished"
