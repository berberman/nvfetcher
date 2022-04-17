{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    Checksum (..),
    Branch (..),
    NixExpr,
    VersionChange (..),
    WithPackageKey (..),

    -- * Nvchecker types
    VersionSortMethod (..),
    ListOptions (..),
    VersionSource (..),
    NvcheckerResult (..),
    NvcheckerRaw (..),
    CheckVersion (..),
    NvcheckerOptions (..),
    UseStaleVersion (..),

    -- * Nix fetcher types
    NixFetcher (..),
    FetchResult,
    FetchStatus (..),

    -- * ExtractSrc Types
    ExtractSrcQ (..),

    -- * FetchRustGitDeps types
    FetchRustGitDepsQ (..),

    -- * Core types
    Core (..),

    -- * Package types
    PackageName,
    PackageFetcher,
    PackageExtractSrc (..),
    PackageCargoLockFiles (..),
    PackagePassthru (..),
    Package (..),
    PackageKey (..),
    PackageResult (..),
  )
where

import qualified Data.Aeson as A
import Data.Coerce (coerce)
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isNothing)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Prettyprinter

--------------------------------------------------------------------------------

-- | Package version
newtype Version = Version Text
  deriving newtype (Eq, Show, Ord, IsString, Semigroup, Monoid, A.FromJSON, A.ToJSON, Pretty)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Check sum, sha256, sri or base32, etc.
newtype Checksum = Checksum Text
  deriving newtype (Show, Eq, Ord, A.FromJSON, A.ToJSON, Pretty)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Git branch ('Nothing': master)
newtype Branch = Branch (Maybe Text)
  deriving newtype (Show, Eq, Ord, Default, Pretty)
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

instance Pretty VersionSortMethod where
  pretty ParseVersion = "ParseVersion"
  pretty Vercmp = "Vercmp"

instance Default VersionSortMethod where
  def = ParseVersion

-- | Filter-like configuration for some version sources.
-- See <https://nvchecker.readthedocs.io/en/latest/usage.html#list-options> for details.
data ListOptions = ListOptions
  { _includeRegex :: Maybe Text,
    _excludeRegex :: Maybe Text,
    _sortVersionKey :: Maybe VersionSortMethod,
    _ignored :: Maybe Text
  }
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData, Default)

isEmptyListOptions :: ListOptions -> Bool
isEmptyListOptions ListOptions {..} =
  isNothing _includeRegex
    && isNothing _excludeRegex
    && isNothing _sortVersionKey
    && isNothing _includeRegex

instance Pretty ListOptions where
  pretty ListOptions {..} =
    "ListOptions"
      <> indent
        2
        ( vsep $
            concat
              [ ppField "includeRegex" _includeRegex,
                ppField "excludeRegex" _excludeRegex,
                ppField "sortVersionKey" _sortVersionKey,
                ppField "ignored" _includeRegex
              ]
        )

-- | Configuration available for evey version sourece.
-- See <https://nvchecker.readthedocs.io/en/latest/usage.html#global-options> for details.
data NvcheckerOptions = NvcheckerOptions
  { _stripPrefix :: Maybe Text,
    _fromPattern :: Maybe Text,
    _toPattern :: Maybe Text
  }
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData, Default)

isEmptyNvcheckerOptions :: NvcheckerOptions -> Bool
isEmptyNvcheckerOptions NvcheckerOptions {..} =
  isNothing _stripPrefix
    && isNothing _fromPattern
    && isNothing _toPattern

instance Pretty NvcheckerOptions where
  pretty NvcheckerOptions {..} =
    "NvcheckerOptions"
      <> line
      <> indent
        2
        ( vsep $
            concat
              [ ppField "stripPrefix" _stripPrefix,
                ppField "fromPattern" _fromPattern,
                ppField "toPattern" _toPattern
              ]
        )

ppField :: Pretty a => Doc ann -> Maybe a -> [Doc ann]
ppField _ Nothing = []
ppField s (Just x) = [s <> colon <+> pretty x]

-- | Upstream version source for nvchecker to check
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
  | OpenVsx {_ovPublisher :: Text, _ovExtName :: Text}
  | VscodeMarketplace {_vsmPublisher :: Text, _vsmExtName :: Text}
  | Cmd {_vcmd :: Text}
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

instance Pretty VersionSource where
  pretty GitHubRelease {..} =
    "CheckGitHubRelease"
      <> line
      <> indent
        2
        ( vsep
            [ "owner" <> colon <+> pretty _owner,
              "repo" <> colon <+> pretty _repo
            ]
        )
  pretty GitHubTag {..} =
    "CheckGitHubTag"
      <> line
      <> indent
        2
        ( vsep $
            [ "owner" <> colon <+> pretty _owner,
              "repo" <> colon <+> pretty _repo
            ]
              <> ["listOptions" <> colon <+> pretty _listOptions | not $ isEmptyListOptions _listOptions]
        )
  pretty Git {..} =
    "CheckGit"
      <> line
      <> indent
        2
        ( vsep
            [ "url" <> colon <+> pretty _vurl,
              "branch" <> colon <+> pretty _vbranch
            ]
        )
  pretty Pypi {..} =
    "CheckPypi" <> colon <+> pretty _pypi
  pretty ArchLinux {..} =
    "CheckArchLinux" <> colon <+> pretty _archpkg
  pretty Aur {..} =
    "CheckAur" <> colon <+> pretty _aur
  pretty Manual {..} =
    "CheckManual" <> colon <+> pretty _manual
  pretty Repology {..} =
    "CheckRepology"
      <> line
      <> indent
        2
        ( vsep
            [ "repology" <> colon <+> pretty _repology,
              "repo" <> colon <+> pretty _repo
            ]
        )
  pretty Webpage {..} =
    "CheckWebpage"
      <> line
      <> indent
        2
        ( vsep $
            [ "url" <> colon <+> pretty _vurl,
              "regex" <> colon <+> pretty _regex
            ]
              <> ["listOptions" <> colon <+> pretty _listOptions | not $ isEmptyListOptions _listOptions]
        )
  pretty HttpHeader {..} =
    "CheckHttpHeader"
      <> line
      <> indent
        2
        ( vsep $
            [ "url" <> colon <+> pretty _vurl,
              "regex" <> colon <+> pretty _regex
            ]
              <> ["listOptions" <> colon <+> pretty _listOptions | not $ isEmptyListOptions _listOptions]
        )
  pretty OpenVsx {..} =
    "CheckOpenVsx"
      <> line
      <> indent
        2
        ( vsep
            [ "publisher" <> colon <+> pretty _ovPublisher,
              "extName" <> colon <+> pretty _ovExtName
            ]
        )
  pretty VscodeMarketplace {..} =
    "CheckVscodeMarketplace"
      <> line
      <> indent
        2
        ( vsep
            [ "publisher" <> colon <+> pretty _vsmPublisher,
              "extName" <> colon <+> pretty _vsmExtName
            ]
        )
  pretty Cmd {..} =
    "CheckCmd" <> colon <+> pretty _vcmd

-- | The input of nvchecker
data CheckVersion = CheckVersion VersionSource NvcheckerOptions
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

instance Pretty CheckVersion where
  pretty (CheckVersion v n) = align (vsep $ [pretty v] <> [pretty n | not $ isEmptyNvcheckerOptions n])

-- | The result of nvchecker rule
data NvcheckerResult = NvcheckerResult
  { nvNow :: Version,
    -- | last result of this nvchecker rule
    -- TODO: consider removing this field
    nvOld :: Maybe Version,
    -- | stale means even 'nvNow' comes from json file (last run)
    -- and we actually didn't run nvchecker this time. 'nvOld' will be 'Nothing' in this case.
    nvStale :: Bool
  }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

-- | Parsed JSON output from nvchecker
newtype NvcheckerRaw = NvcheckerRaw Version
  deriving (Show, Typeable, Eq, Generic)

instance A.FromJSON NvcheckerRaw where
  parseJSON = A.withObject "NvcheckerRaw" $ \o ->
    NvcheckerRaw <$> o A..: "version"

type instance RuleResult CheckVersion = NvcheckerResult

--------------------------------------------------------------------------------

-- | If the package is prefetched, then we can obtain the SHA256
data NixFetcher (k :: FetchStatus)
  = FetchGit
      { _furl :: Text,
        _rev :: Version,
        _deepClone :: Bool,
        _fetchSubmodules :: Bool,
        _leaveDotGit :: Bool,
        _name :: Maybe Text,
        _sha256 :: FetchResult k
      }
  | FetchGitHub
      { _fowner :: Text,
        _frepo :: Text,
        _rev :: Version,
        _deepClone :: Bool,
        _fetchSubmodules :: Bool,
        _leaveDotGit :: Bool,
        _name :: Maybe Text,
        _sha256 :: FetchResult k
      }
  | FetchUrl
      { _furl :: Text,
        _name :: Maybe Text,
        _sha256 :: FetchResult k
      }
  | FetchTarball
      { _furl :: Text,
        _sha256 :: FetchResult k
      }
  deriving (Typeable, Generic)

-- | Fetch status
data FetchStatus = Fresh | Fetched

-- | Prefetched fetchers hold hashes
type family FetchResult (k :: FetchStatus) where
  FetchResult Fresh = ()
  FetchResult Fetched = Checksum

type instance RuleResult (NixFetcher Fresh) = NixFetcher Fetched

deriving instance Show (FetchResult k) => Show (NixFetcher k)

deriving instance Eq (FetchResult k) => Eq (NixFetcher k)

deriving instance Ord (FetchResult k) => Ord (NixFetcher k)

deriving instance Hashable (FetchResult k) => Hashable (NixFetcher k)

deriving instance Binary (FetchResult k) => Binary (NixFetcher k)

deriving instance NFData (FetchResult k) => NFData (NixFetcher k)

instance A.ToJSON (NixFetcher Fetched) where
  toJSON FetchGit {..} =
    A.object
      [ "url" A..= _furl,
        "rev" A..= _rev,
        "deepClone" A..= _deepClone,
        "fetchSubmodules" A..= _fetchSubmodules,
        "leaveDotGit" A..= _leaveDotGit,
        "name" A..= _name,
        "sha256" A..= _sha256,
        "type" A..= A.String "git"
      ]
  toJSON FetchGitHub {..} =
    A.object
      [ "owner" A..= _fowner,
        "repo" A..= _frepo,
        "rev" A..= _rev,
        "deepClone" A..= _deepClone,
        "fetchSubmodules" A..= _fetchSubmodules,
        "leaveDotGit" A..= _leaveDotGit,
        "name" A..= _name,
        "sha256" A..= _sha256,
        "type" A..= A.String "github"
      ]
  toJSON FetchUrl {..} =
    A.object
      [ "url" A..= _furl,
        "name" A..= _name,
        "sha256" A..= _sha256,
        "type" A..= A.String "url"
      ]
  toJSON FetchTarball {..} =
    A.object
      [ "url" A..= _furl,
        "sha256" A..= _sha256,
        "type" A..= A.String "tarball"
      ]

instance Pretty (NixFetcher k) where
  pretty FetchGit {..} =
    "FetchGit"
      <> line
      <> indent
        2
        ( vsep $
            [ "url" <> colon <+> pretty _furl,
              "rev" <> colon <+> pretty _rev,
              "deepClone" <> colon <+> pretty _deepClone,
              "fetchSubmodules" <> colon <+> pretty _fetchSubmodules,
              "leaveDotGit" <> colon <+> pretty _leaveDotGit
            ]
              <> ppField "name" _name
        )
  pretty FetchGitHub {..} =
    "FetchGitHub"
      <> line
      <> indent
        2
        ( vsep $
            [ "owner" <> colon <+> pretty _fowner,
              "repo" <> colon <+> pretty _frepo,
              "rev" <> colon <+> pretty _rev,
              "deepClone" <> colon <+> pretty _deepClone,
              "fetchSubmodules" <> colon <+> pretty _fetchSubmodules,
              "leaveDotGit" <> colon <+> pretty _leaveDotGit
            ]
              <> ppField "name" _name
        )
  pretty FetchUrl {..} =
    "FetchUrl"
      <> line
      <> indent
        2
        ( vsep $
            ["url" <> colon <+> pretty _furl]
              <> ppField "name" _name
        )
  pretty FetchTarball {..} =
    "FetchTarball" <> colon <+> pretty _furl

--------------------------------------------------------------------------------

-- | Extract file contents from package source
-- e.g. @Cargo.lock@
data ExtractSrcQ = ExtractSrcQ (NixFetcher Fetched) (NE.NonEmpty FilePath)
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

type instance RuleResult ExtractSrcQ = HashMap FilePath Text

instance Pretty ExtractSrcQ where
  pretty (ExtractSrcQ f n) =
    "ExtractSrc" <> line
      <> indent
        2
        ( vsep
            [ "fetcher" <> colon <+> pretty f,
              "files" <> colon <+> pretty n
            ]
        )

--------------------------------------------------------------------------------

-- | Fetch @outputHashes@ for git dependencies in @Cargo.lock@.
-- See <https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#importing-a-cargolock-file> for details.
-- We need fetched source and the file path to @Cargo.lock@.
data FetchRustGitDepsQ = FetchRustGitDepsQ (NixFetcher Fetched) FilePath
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

-- | @outputHashes@, a mapping from nameVer -> output hash
type instance RuleResult FetchRustGitDepsQ = HashMap Text Checksum

instance Pretty FetchRustGitDepsQ where
  pretty (FetchRustGitDepsQ f n) =
    "FetchRustGitDeps" <> line
      <> indent
        2
        ( vsep
            [ "fetcher" <> colon <+> pretty f,
              "cargoLock" <> colon <+> pretty n
            ]
        )

--------------------------------------------------------------------------------

-- | Package name, used in generating nix expr
type PackageName = Text

-- | How to create package source fetcher given a version
type PackageFetcher = Version -> NixFetcher Fresh

newtype PackageExtractSrc = PackageExtractSrc (NE.NonEmpty FilePath)

newtype PackageCargoLockFiles = PackageCargoLockFiles (NE.NonEmpty FilePath)

newtype PackagePassthru = PackagePassthru (HashMap Text Text)
  deriving newtype (Semigroup, Monoid)

-- | Using stale value indicates that we will /NOT/ check for new versions if
-- there is a known version recoverd from json file or last use of the rule.
-- Normally you don't want a stale version
-- unless you need pin a package.
data UseStaleVersion
  = -- | Specified in configuration file
    PermanentStale
  | -- | Specified by @--filter@ command
    TemporaryStale
  | NoStale
  deriving stock (Eq, Show, Ord, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | A package is defined with:
--
-- 1. its name
-- 2. how to track its version
-- 3. how to fetch it as we have the version
-- 4. optional file paths to extract (dump to build dir)
-- 5. optional @Cargo.lock@ path (if it's a rust package)
-- 6. an optional pass through map
-- 7. if the package version was pinned
--
-- /INVARIANT: 'Version' passed to 'PackageFetcher' MUST be used textually,/
-- /i.e. can only be concatenated with other strings,/
-- /in case we can't check the equality between fetcher functions./
data Package = Package
  { _pname :: PackageName,
    _pversion :: CheckVersion,
    _pfetcher :: PackageFetcher,
    _pextract :: Maybe PackageExtractSrc,
    _pcargo :: Maybe PackageCargoLockFiles,
    _ppassthru :: PackagePassthru,
    _ppinned :: UseStaleVersion
  }

-- | Package key is the name of a package.
-- We use this type to index packages.
newtype PackageKey = PackageKey PackageName
  deriving newtype (Eq, Show, Ord, Pretty)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

--------------------------------------------------------------------------------

-- | The key type of nvfetcher rule. See "NvFetcher.Core"
data Core = Core
  deriving (Eq, Show, Ord, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult Core = PackageResult

-- | Decorate a rule's key with 'PackageKey'
newtype WithPackageKey k = WithPackageKey (k, PackageKey)
  deriving newtype (Eq, Hashable, Binary, NFData)

instance Show k => Show (WithPackageKey k) where
  show (WithPackageKey (k, n)) = show k <> " (" <> show n <> ")"

type instance RuleResult (WithPackageKey k) = RuleResult k

-- | Result type of 'Core'
data PackageResult = PackageResult
  { _prname :: PackageName,
    _prversion :: NvcheckerResult,
    _prfetched :: NixFetcher 'Fetched,
    _prpassthru :: Maybe (HashMap Text Text),
    -- | extracted file name -> file path in build dir
    _prextract :: Maybe (HashMap FilePath NixExpr),
    -- | cargo lock file path in build dir -> (file path in nix, git dependencies)
    _prcargolock :: Maybe (HashMap FilePath (NixExpr, HashMap Text Checksum)),
    _prpinned :: UseStaleVersion
  }
  deriving (Show, Typeable, Generic, NFData)

instance A.ToJSON PackageResult where
  toJSON PackageResult {..} =
    A.object
      [ "name" A..= _prname,
        "version" A..= nvNow _prversion,
        "src" A..= _prfetched,
        "extract" A..= _prextract,
        "passthru" A..= _prpassthru,
        "cargoLocks" A..= _prcargolock,
        "pinned" A..= case _prpinned of
          PermanentStale -> True
          _ -> False
      ]
