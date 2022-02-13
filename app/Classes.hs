{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Classes (
    Author,
    Project
) where

import Rel8
import Data.Text
import GHC.Generics
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

deriving stock instance f ~ Result => Show (Author f)
deriving stock instance f ~ Result => Show (Project f)