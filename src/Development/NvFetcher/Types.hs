{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.NvFetcher.Types where

import qualified Data.Aeson as A
import Data.String (IsString)
import Data.Text (Text)
import Development.Shake.Classes
import GHC.Generics (Generic)

newtype Version = Version Text
  deriving newtype (Eq, Show, Ord, IsString, Semigroup, Monoid, A.FromJSON, A.ToJSON)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

newtype SHA256 = SHA256 Text
  deriving newtype (Show, Eq)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)
