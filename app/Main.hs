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
import Hasql.Connection ( Connection )
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction)
import Control.Monad.IO.Class
import qualified Control.Exception as Servant
import Hasql.Pool
import Control.Monad.Reader
import Hasql.Session
import Text.Read (readMaybe)

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

deriving stock instance f ~ Result => Show (Author f)
deriving stock instance f ~ Result => Show (Project f)


main :: IO ()
main = return ()