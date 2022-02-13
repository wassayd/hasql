{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module ProjectRepository(
  projectSchema,
  getAllProject,
  createProject,
  deleteProject
) where

import Database
import Classes
import GHC.Generics
import Data.Text
import Rel8
import Data.Int

newtype AuthorId = AuthorId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show)

data Author f = Author
  { author_id :: Column f AuthorId
  , author_name     :: Column f Text
  , author_url      :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

data Project f = Project
  { projectAuthorId :: Column f AuthorId
  , projectName     :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Classes.Author f)
deriving stock instance f ~ Result => Show (Classes.Project f)


projectSchema :: TableSchema (Classes.Project Name)
projectSchema = TableSchema
  { name = "project"
  , schema = Nothing
  , columns = Project
      { projectAuthorId = "author_id"
      , projectName = "project_name"
      }
  }


getAllProject :: Query (Classes.Project Expr)
getAllProject = each projectSchema


createProject :: Int64 -> String -> IO ()
createProject authorId prjName = do 
  Right conn <- dbConnection
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
  Right conn <- dbConnection
  let del = Delete { from = authorSchema
                   , using = getAllAuthor
                   , deleteWhere = \_ row -> author_id row ==. litExpr (AuthorId auth_id)
                   , returning = NumberOfRowsAffected
                   }
  res <- run (statement () (delete del)) conn
  either printErr (printRes "Records deleted: ") res
  putStrLn "Finished"
