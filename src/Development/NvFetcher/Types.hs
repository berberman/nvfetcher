{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  = GitHub {owner :: Text, repo :: Text}
  | Pypi {pypi :: Text}
  | ArchLinux {archpkg :: Text}
  | Aur {aur :: Text}
  | Manual {manual :: Text}
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult VersionSource = Version

data NixFetcher (k :: Prefetch)
  = FetchFromGitHub
      { gitHubOwner :: Text,
        gitHubRepo :: Text,
        gitHubRev :: Version,
        sha256 :: MapPrefetch k
      }
  | FetchUrl {url :: Text, sha256 :: MapPrefetch k}
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
  show x = "Package{name = " <> T.unpack (pname x) <> "}"

instance Eq Package where
  (==) = (==) `on` pname

instance Ord Package where
  compare = compare `on` pname
