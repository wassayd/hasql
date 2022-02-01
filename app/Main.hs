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

import Prelude
import Rel8
import Data.Int (Int64)
import GHC.Generics ( Generic )
import Data.Text ( Text )
import Hasql.Connection
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Data.ByteString.UTF8 as StrictUTF8 (fromString, toString)

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

 
main :: IO ()
main = do
  connection <- acquire connectionSettings
  case connection of 
    Left (Just a) -> error $ StrictUTF8.toString a
    Left Nothing -> error ""
    Right connectionRes -> do
      putStrLn "Acquired connection!"
      Hasql.Connection.release connectionRes
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "root" "postgres"
  