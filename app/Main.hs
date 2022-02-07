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

newtype AuthorId = AuthorId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show)

newtype AppEnv = AppEnv
    { envDbPool :: Pool
    }

type AppM = AppEnv

class HasDbPool env where
  getDbPool :: env -> Pool

runHandler env app = Servant.Handler $ ExceptT $ try $ runRIO env app

instance HasDbPool AppEnv where
    getDbPool = envDbPool



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



runStmt :: Statement () a -> Transaction a
runStmt = statement ()


runTransaction :: forall a m env . (HasDbPool env, MonadReader env m, MonadIO m) => Transaction a -> m a
runTransaction transaction = do
    pool <- getDbPool
    runTransactionWithPool pool transaction
    
executeStmt = runTransaction . runStmt

getAllProject :: Query (Project Expr)
getAllProject = each projectSchema

getAllAuthor :: Query (Author Expr)
getAllAuthor = each authorSchema