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

module Development.NvFetcher.Types
  ( -- * Common types
    Version (..),
    SHA256 (..),

    -- * Nvchecker types
    VersionSource (..),
    NvcheckerResult (..),

    -- * Nix fetcher types
    NixFetcher (..),
    Prefetch (..),

    -- * Package types
    PackageName,
    PackageFetcher,
    Package (..),
  )
where

import qualified Data.Aeson as A
import Data.Function (on)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)

-- | Package version
newtype Version = Version Text
  deriving newtype (Eq, Show, Ord, IsString, Semigroup, Monoid, A.FromJSON, A.ToJSON)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | SHA 256 sum
newtype SHA256 = SHA256 Text
  deriving newtype (Show, Eq)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | The input of nvchecker
data VersionSource
  = GitHubRelease {owner :: Text, repo :: Text}
  | Git {vurl :: Text}
  | Pypi {pypi :: Text}
  | ArchLinux {archpkg :: Text}
  | Aur {aur :: Text}
  | Manual {manual :: Text}
  | Repology {repology :: Text, repo :: Text}
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

-- | The result of running nvchecker
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

-- | Package fetcher
-- if the package is prefetched, then we can obtain the SHA256
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

-- | Prefetch status
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

-- | Package name, used in generating nix expr
type PackageName = Text

-- | How to create package source fetcher given a version
type PackageFetcher = Version -> NixFetcher Fresh

-- | A package is defined with its name, how to track its version, and how to fetch it as we have version
data Package = Package
  { pname :: PackageName,
    pversion :: VersionSource,
    pfetcher :: PackageFetcher
  }

instance Show Package where
  show Package {..} = "Package {name = " <> T.unpack pname <> ", version = " <> show pversion <> ", fetcher = {..}" <> "}"

instance Eq Package where
  (==) = (==) `on` pname

instance Ord Package where
  compare = compare `on` pname
