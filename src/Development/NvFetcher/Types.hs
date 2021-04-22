{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.NvFetcher.Types where

import qualified Data.Aeson as A
import Data.Function (on)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
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

data VersionSource
  = GitHubRelease {owner :: Text, repo :: Text}
  | Git {vurl :: Text}
  | Pypi {pypi :: Text}
  | ArchLinux {archpkg :: Text}
  | Aur {aur :: Text}
  | Manual {manual :: Text}
  | Repology {repology :: Text, repo :: Text}
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

data NvcheckerResult = NvcheckerResult
  { nvNow :: Version,
    -- | nvchecker doesn't give this value, but shake restores it from last run
    nvOld :: Maybe Version
  }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

instance A.FromJSON NvcheckerResult where
  parseJSON = A.withObject "NvcheckerResult" $ \o ->
    NvcheckerResult <$> o A..: "version" <*> pure Nothing

type instance RuleResult VersionSource = NvcheckerResult

data NixFetcher (k :: Prefetch)
  = FetchGit
      { furl :: Text,
        rev :: Version,
        branch :: Maybe Text,
        deepClone :: Bool,
        fetchSubmodules :: Bool,
        leaveDotGit :: Bool,
        sha256 :: MapPrefetch k
      }
  | FetchUrl {furl :: Text, sha256 :: MapPrefetch k}
  deriving (Typeable, Generic)

data Prefetch = Fresh | Prefetched

type family MapPrefetch (k :: Prefetch) where
  MapPrefetch Fresh = ()
  MapPrefetch Prefetched = SHA256

type instance RuleResult (NixFetcher Fresh) = NixFetcher Prefetched

deriving instance Show (MapPrefetch k) => Show (NixFetcher k)

deriving instance Eq (MapPrefetch k) => Eq (NixFetcher k)

deriving instance Hashable (MapPrefetch k) => Hashable (NixFetcher k)

deriving instance Binary (MapPrefetch k) => Binary (NixFetcher k)

deriving instance NFData (MapPrefetch k) => NFData (NixFetcher k)

type PackageName = Text

data Package = Package
  { pname :: PackageName,
    pversion :: VersionSource,
    pfetcher :: Version -> NixFetcher Fresh
  }

instance Show Package where
  show Package {..} = "Package {name = " <> T.unpack pname <> ", version = " <> show pversion <> ", fetcher = {..}" <> "}"

instance Eq Package where
  (==) = (==) `on` pname

instance Ord Package where
  compare = compare `on` pname
