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

module ProjectRepository(
  projectSchema,
  getAllProject,
  createProject,
  deleteProject
) where

import Database ( dbConnection, printErr, printRes, runStatement  )
import Classes
import GHC.Generics
import Data.Text
import Rel8
import Data.Int
import Hasql.Session

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
  Right conn <- dbConnection
  let nameConv  = pack prjName :: Column Result Text
  let ins = Insert { into = projectSchema
                   , rows = values [ lit Project { projectAuthorId = Classes.AuthorId authorId, projectName = nameConv  } ]
                   , onConflict = Abort
                   , returning = NumberOfRowsAffected
                   }
  res <- runStatement () (insert ins) conn
  either printErr (printRes "Records inserted: ") res
  putStrLn "Finished"



updateProject :: Int64 -> IO ()
updateProject auth_id =  do
  Right conn <- dbConnection
  let upd = Update { target = projectSchema
                   , from = getAllProject
                   , set = \_ row -> row { projectName = "https://www.google.fr/" }
                   , updateWhere = \_ row -> projectAuthorId row ==. litExpr (AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (update upd)) conn
  either printErr (printRes "Records updated: ") res
  putStrLn "Finished"


deleteProject :: Int64 -> IO()
deleteProject auth_id = do
  Right conn <- dbConnection
  let del = Delete { from = projectSchema
                   , using = getAllProject
                   , deleteWhere = \_ row -> projectAuthorId row ==. litExpr (Classes.AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (delete del)) conn
  either printErr (printRes "Records deleted: ") res
  putStrLn "Finished"

