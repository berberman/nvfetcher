{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- Types used in this program.
module NvFetcher.Types
  ( -- * Common types
    Version (..),
    SHA256 (..),
    Branch (..),
    NixExpr,
    VersionChange (..),
    WithPackageKey (..),

    -- * Nvchecker types
    VersionSortMethod (..),
    ListOptions (..),
    VersionSource (..),
    NvcheckerResult (..),

    -- * Nix fetcher types
    NixFetcher (..),
    FetchResult,
    FetchStatus (..),

    -- * ExtractSrc Types
    ExtractSrc (..),

    -- * FetchRustGitDeps types
    FetchRustGitDeps (..),

    -- * Core types
    Core (..),

    -- * Package types
    PackageName,
    PackageFetcher,
    Package (..),
    PackageKey (..),
  )
where

import qualified Data.Aeson as A
import Data.Coerce (coerce)
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Package version
newtype Version = Version Text
  deriving newtype (Eq, Show, Ord, IsString, Semigroup, Monoid, A.FromJSON, A.ToJSON)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | SHA 256 sum
newtype SHA256 = SHA256 Text
  deriving newtype (Show, Eq, Ord)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Git branch ('Nothing': master)
newtype Branch = Branch (Maybe Text)
  deriving newtype (Show, Eq, Ord, Default)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Version change of a package
--
-- >>> VersionChange "foo" Nothing "2.3.3"
-- foo: ∅ → 2.3.3
--
-- >>> VersionChange "bar" (Just "2.2.2") "2.3.3"
-- bar: 2.2.2 → 2.3.3
data VersionChange = VersionChange
  { vcName :: PackageName,
    vcOld :: Maybe Version,
    vcNew :: Version
  }
  deriving (Eq)

instance Show VersionChange where
  show VersionChange {..} =
    T.unpack $ vcName <> ": " <> fromMaybe "∅" (coerce vcOld) <> " → " <> coerce vcNew

-- | Nix expression
type NixExpr = Text

--------------------------------------------------------------------------------

data VersionSortMethod = ParseVersion | Vercmp
  deriving (Typeable, Eq, Ord, Enum, Generic, Hashable, Binary, NFData)

instance Show VersionSortMethod where
  show = \case
    ParseVersion -> "parse_version"
    Vercmp -> "vercmp"

instance Default VersionSortMethod where
  def = ParseVersion

-- | The extra configuration for some version sources.
-- See <https://nvchecker.readthedocs.io/en/latest/usage.html#list-options> for details.
data ListOptions = ListOptions
  { _includeRegex :: Maybe Text,
    _excludeRegex :: Maybe Text,
    _sortVersionKey :: Maybe VersionSortMethod,
    _ignored :: Maybe Text
  }
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData, Default)

-- | The input of nvchecker
data VersionSource
  = GitHubRelease {_owner :: Text, _repo :: Text}
  | GitHubTag {_owner :: Text, _repo :: Text, _listOptions :: ListOptions}
  | Git {_vurl :: Text, _vbranch :: Branch}
  | Pypi {_pypi :: Text}
  | ArchLinux {_archpkg :: Text}
  | Aur {_aur :: Text}
  | Manual {_manual :: Text}
  | Repology {_repology :: Text, _repo :: Text}
  | Webpage {_vurl :: Text, _regex :: Text, _listOptions :: ListOptions}
  | HttpHeader {_vurl :: Text, _regex :: Text, _listOptions :: ListOptions}
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

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

--------------------------------------------------------------------------------

-- | If the package is prefetched, then we can obtain the SHA256
data NixFetcher (k :: FetchStatus)
  = FetchGit
      { _furl :: Text,
        _rev :: Version,
        _branch :: Branch,
        _deepClone :: Bool,
        _fetchSubmodules :: Bool,
        _leaveDotGit :: Bool,
        _sha256 :: FetchResult k
      }
  | FetchUrl {_furl :: Text, _sha256 :: FetchResult k}
  deriving (Typeable, Generic)

-- | Fetch status
data FetchStatus = Fresh | Fetched

-- | Prefetched fetchers hold hashes
type family FetchResult (k :: FetchStatus) where
  FetchResult Fresh = ()
  FetchResult Fetched = SHA256

type instance RuleResult (NixFetcher Fresh) = NixFetcher Fetched

deriving instance Show (FetchResult k) => Show (NixFetcher k)

deriving instance Eq (FetchResult k) => Eq (NixFetcher k)

deriving instance Ord (FetchResult k) => Ord (NixFetcher k)

deriving instance Hashable (FetchResult k) => Hashable (NixFetcher k)

deriving instance Binary (FetchResult k) => Binary (NixFetcher k)

deriving instance NFData (FetchResult k) => NFData (NixFetcher k)

--------------------------------------------------------------------------------

-- | Extract file contents from package source
-- e.g. @Cargo.lock@
data ExtractSrc = ExtractSrc (NixFetcher Fetched) [FilePath]
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

type instance RuleResult ExtractSrc = HashMap FilePath Text

--------------------------------------------------------------------------------

-- | Fetch @outputHashes@ for git dependencies in @Cargo.lock@.
-- See <https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#importing-a-cargolock-file> for details.
-- We need fetched source and the file path to @@Cargo.lock@.
data FetchRustGitDeps = FetchRustGitDeps (NixFetcher Fetched) FilePath
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

-- | @outputHashes@, a mapping from nameVer -> output hash
type instance RuleResult FetchRustGitDeps = HashMap Text SHA256

--------------------------------------------------------------------------------

-- | Package name, used in generating nix expr
type PackageName = Text

-- | How to create package source fetcher given a version
type PackageFetcher = Version -> NixFetcher Fresh

-- | A package is defined with:
--
-- 1. its name
-- 2. how to track its version
-- 3. how to fetch it as we have the version
-- 4. optional file paths to extract (dump to generated nix expr)
-- 5. @Cargo.lock@ path (if it's a rust package)
--
-- /INVARIANT: 'Version' passed to 'PackageFetcher' MUST be used textually,/
-- /i.e. can only be concatenated with other strings,/
-- /in case we can't check the equality between fetcher functions./
data Package = Package
  { _pname :: PackageName,
    _pversion :: VersionSource,
    _pfetcher :: PackageFetcher,
    _pextract :: [FilePath],
    _pcargo :: Maybe FilePath
  }

-- | Package key is the name of a package.
-- We use this type to index packages.
newtype PackageKey = PackageKey PackageName
  deriving newtype (Eq, Show, Ord)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

--------------------------------------------------------------------------------

-- | The key type of nvfetcher rule. See "NvFetcher.Core"
data Core = Core
  deriving (Eq, Show, Ord, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult Core = NixExpr

-- | Decorate a rule's key with 'PackageKey'
newtype WithPackageKey k = WithPackageKey (k, PackageKey)
  deriving newtype (Eq, Hashable, Binary, NFData)

instance Show k => Show (WithPackageKey k) where
  show (WithPackageKey (k, n)) = show k <> " (" <> show n <> ")"

type instance RuleResult (WithPackageKey k) = RuleResult k
