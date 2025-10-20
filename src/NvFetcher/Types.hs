{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Copyright: (c) 2021-2025 berberman
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
    ContainerDigest (..),
    Branch (..),
    NixExpr,
    VersionChange (..),
    WithPackageKey (..),
    Glob (..),

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
    RunFetch (..),
    ForceFetch (..),
    NixFetcher (..),
    FetchResult,
    FetchStatus (..),

    -- * ExtractSrc Types
    ExtractSrcQ (..),

    -- * FetchRustGitDeps types
    FetchRustGitDepsQ (..),

    -- * GetGitCommitDate types
    GitDateFormat (..),
    GetGitCommitDate (..),
    GitTimeZone (..),

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
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prettyprinter

--------------------------------------------------------------------------------

-- | Helper type for generating 'Show' and 'Pretty' instances
-- Type level string @d@ denotes the string used for the default value 'Nothing'
newtype DefaultableText (d :: Symbol) = DefaultableText (Maybe Text)

instance (KnownSymbol d) => Show (DefaultableText d) where
  show (DefaultableText Nothing) = "default (" <> symbolVal (Proxy :: Proxy d) <> ")"
  show (DefaultableText x) = show x

instance (KnownSymbol d) => Pretty (DefaultableText d) where
  pretty (DefaultableText Nothing) = pretty $ "default (" <> symbolVal (Proxy :: Proxy d) <> ")"
  pretty (DefaultableText (Just x)) = pretty x

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

-- | Digest of a (Docker) container
newtype ContainerDigest = ContainerDigest Text
  deriving newtype (Show, Eq, Ord, A.FromJSON, A.ToJSON, Pretty)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Git branch ('Nothing': master)
newtype Branch = Branch (Maybe Text)
  deriving newtype (Eq, Ord, Default)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)
  deriving (Pretty, Show) via DefaultableText "master"

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

data VersionSortMethod = ParseVersion | Vercmp | AwesomeVersion
  deriving (Typeable, Eq, Ord, Enum, Generic, Hashable, Binary, NFData)

instance Show VersionSortMethod where
  show = \case
    ParseVersion -> "parse_version"
    Vercmp -> "vercmp"
    AwesomeVersion -> "awesomeversion"

instance Pretty VersionSortMethod where
  pretty ParseVersion = "ParseVersion"
  pretty Vercmp = "Vercmp"
  pretty AwesomeVersion = "AwesomeVersion"

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

ppField :: (Pretty a) => Doc ann -> Maybe a -> [Doc ann]
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
  | Container {_vcontainer :: Text, _listOptions :: ListOptions}
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
  pretty Container {..} =
    "CheckContainer" <> colon <+> pretty _vcontainer

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
data NvcheckerRaw = NvcheckerSuccess Version | NvcheckerError Text
  deriving (Show, Typeable, Eq, Generic)

instance A.FromJSON NvcheckerRaw where
  parseJSON = A.withObject "NvcheckerRaw" $ \o -> do
    mVersion <- o A..:? "version"
    case mVersion of
      Just version -> pure $ NvcheckerSuccess version
      _ -> NvcheckerError <$> o A..: "error"

type instance RuleResult CheckVersion = NvcheckerResult

--------------------------------------------------------------------------------

-- | Whether to cache the fetched sha256
--
-- @ForceFetch@ indicates @alwaysRerun@ the fetcher rule
data ForceFetch = ForceFetch | NoForceFetch
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

instance Pretty ForceFetch where
  pretty ForceFetch = "ForceFetch"
  pretty NoForceFetch = "NoForceFetch"

instance Default ForceFetch where
  def = NoForceFetch

-- | The input of prefetch rule
data RunFetch = RunFetch ForceFetch (NixFetcher Fresh)
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

-- Prefetch rule never throws exceptions
type instance RuleResult RunFetch = Maybe (NixFetcher Fetched)

-- | If the package is prefetched, then we can obtain the SHA256
data NixFetcher (k :: FetchStatus)
  = FetchGit
      { _furl :: Text,
        _rev :: Version,
        _deepClone :: Bool,
        _fetchSubmodules :: Bool,
        _leaveDotGit :: Bool,
        _sparseCheckout :: [Text],
        _name :: Maybe Text,
        _sha256 :: FetchResult Checksum k
      }
  | FetchGitHub
      { _fowner :: Text,
        _frepo :: Text,
        _rev :: Version,
        _deepClone :: Bool,
        _fetchSubmodules :: Bool,
        _leaveDotGit :: Bool,
        _sparseCheckout :: [Text],
        _name :: Maybe Text,
        _sha256 :: FetchResult Checksum k
      }
  | FetchUrl
      { _furl :: Text,
        _name :: Maybe Text,
        _sha256 :: FetchResult Checksum k
      }
  | FetchTarball
      { _furl :: Text,
        _sha256 :: FetchResult Checksum k
      }
  | FetchDocker
      { _imageName :: Text,
        _imageTag :: Text,
        _imageDigest :: FetchResult ContainerDigest k,
        _sha256 :: FetchResult Checksum k,
        _fos :: Maybe Text,
        _farch :: Maybe Text,
        _finalImageName :: Maybe Text,
        _finalImageTag :: Maybe Text,
        _tlsVerify :: Maybe Bool
      }
  deriving (Typeable, Generic)

class (c (FetchResult Checksum k), c (FetchResult ContainerDigest k)) => ForFetchResult c k

instance (c (FetchResult Checksum k), c (FetchResult ContainerDigest k)) => ForFetchResult c k

deriving instance (Show `ForFetchResult` k) => Show (NixFetcher k)

deriving instance (Eq `ForFetchResult` k) => Eq (NixFetcher k)

deriving instance (Ord `ForFetchResult` k) => Ord (NixFetcher k)

deriving instance (Hashable `ForFetchResult` k) => Hashable (NixFetcher k)

deriving instance (Binary `ForFetchResult` k) => Binary (NixFetcher k)

deriving instance (NFData `ForFetchResult` k) => NFData (NixFetcher k)

-- | Fetch status
data FetchStatus = Fresh | Fetched

-- | Prefetched fetchers hold hashes
type family FetchResult a (k :: FetchStatus) where
  FetchResult _ Fresh = ()
  FetchResult a Fetched = a

instance A.ToJSON (NixFetcher Fetched) where
  toJSON FetchGit {..} =
    A.object
      [ "url" A..= _furl,
        "rev" A..= _rev,
        "deepClone" A..= _deepClone,
        "fetchSubmodules" A..= _fetchSubmodules,
        "leaveDotGit" A..= _leaveDotGit,
        "sparseCheckout" A..= _sparseCheckout,
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
        "sparseCheckout" A..= _sparseCheckout,
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
  toJSON FetchDocker {..} =
    A.object
      [ "imageName" A..= _imageName,
        "imageTag" A..= _imageTag,
        "imageDigest" A..= _imageDigest,
        "sha256" A..= _sha256,
        "os" A..= _fos,
        "arch" A..= _farch,
        "finalImageName" A..= _finalImageName,
        "finalImageTag" A..= _finalImageTag,
        "tlsVerify" A..= _tlsVerify
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
              "leaveDotGit" <> colon <+> pretty _leaveDotGit,
              "sparseCheckout" <> colon <+> pretty _sparseCheckout
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
              "leaveDotGit" <> colon <+> pretty _leaveDotGit,
              "sparseCheckout" <> colon <+> pretty _sparseCheckout
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
  pretty FetchDocker {..} =
    "FetchDocker"
      <> line
      <> indent
        2
        ( vsep $
            [ "imageName" <> colon <+> pretty _imageName,
              "imageTag" <> colon <+> pretty _finalImageTag
            ]
              <> ppField "os" _fos
              <> ppField "arch" _farch
              <> ppField "finalImageName" _finalImageName
              <> ppField "finalImageTag" _finalImageTag
              <> ppField "tlsVerify" _tlsVerify
        )

--------------------------------------------------------------------------------

-- | Zsh style glob pattern
-- Notably, recursive wildcards like @**/@ are supported.
newtype Glob = Glob FilePath
  deriving newtype (Show, Eq, Ord, IsString, Pretty)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Extract file contents from package source
-- Matched files will be copied to build dir.
-- All matched directories are ignored. Only files will be processed.
data ExtractSrcQ = ExtractSrcQ (NixFetcher Fetched) (NE.NonEmpty Glob)
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

-- | Rule result for extracting source files. 'Text' is the file contents,
-- only available if the second element of the tuple in 'ExtractSrcQ' is @True@.
-- The key of the result map is the file path relative to the package source,
-- and the value is the file path relative to the build directory.
type instance RuleResult ExtractSrcQ = HashMap FilePath FilePath

instance Pretty ExtractSrcQ where
  pretty (ExtractSrcQ f n) =
    "ExtractSrc"
      <> line
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
    "FetchRustGitDeps"
      <> line
      <> indent
        2
        ( vsep
            [ "fetcher" <> colon <+> pretty f,
              "cargoLock" <> colon <+> pretty n
            ]
        )

--------------------------------------------------------------------------------

-- | @strftime@ format
-- Defaults to @%Y-%m-%d@
newtype GitDateFormat = GitDateFormat (Maybe Text)
  deriving newtype (Eq, Ord, Default)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)
  deriving (Pretty, Show) via DefaultableText "%Y-%m-%d"

-- | Defaults to commit's time zone.
-- When set to @local@, current local time zone is used.
-- Only used in 'GetGitCommitDate'.
newtype GitTimeZone = GitTimeZone (Maybe Text)
  deriving newtype (Eq, Ord, Default)
  deriving stock (Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)
  deriving (Pretty, Show) via DefaultableText "commit's time zone"

-- | Get the commit date by using shallow clone
--
-- @_gformat@ is in.
-- Note: Requires git >= 2.5
data GetGitCommitDate = GetGitCommitDate
  { _gurl :: Text,
    _grev :: Text,
    _gformat :: (GitDateFormat, GitTimeZone)
  }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

type instance RuleResult GetGitCommitDate = Text

instance Pretty GetGitCommitDate where
  pretty GetGitCommitDate {..} =
    "GetGitCommitDate"
      <> line
      <> indent
        2
        ( vsep
            [ "url" <> colon <+> pretty _gurl,
              "rev" <> colon <+> pretty _grev,
              "format" <> colon <+> pretty _gformat
            ]
        )

--------------------------------------------------------------------------------

-- | Package name, used in generating nix expr
type PackageName = Text

-- | How to create package source fetcher given a version
type PackageFetcher = Version -> NixFetcher Fresh

newtype PackageExtractSrc = PackageExtractSrc (NE.NonEmpty Glob)

newtype PackageCargoLockFiles = PackageCargoLockFiles (NE.NonEmpty Glob)

newtype PackagePassthru = PackagePassthru (HashMap Text Text)
  deriving newtype (Semigroup, Monoid)

-- | Using stale value indicates that we will /NOT/ check for new versions if
-- there is a known version recovered from json file or last use of the rule.
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
-- 5. optional @Cargo.lock@ paths (if it's a rust package)
-- 6. an optional pass through map
-- 7. if the package version was pinned
-- 8. optional git date format with time zone (if the version source is git)
-- 9. whether to always fetch a package regardless of the version changing
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
    _ppinned :: UseStaleVersion,
    _pgitdate :: (GitDateFormat, GitTimeZone),
    _pforcefetch :: ForceFetch
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

-- If prefetch fails, we don't want to fail the whole build
type instance RuleResult Core = Maybe PackageResult

-- | Decorate a rule's key with 'PackageKey'
newtype WithPackageKey k = WithPackageKey (k, PackageKey)
  deriving newtype (Eq, Hashable, Binary, NFData)

instance (Show k) => Show (WithPackageKey k) where
  show (WithPackageKey (k, n)) = show k <> " (" <> show n <> ")"

type instance RuleResult (WithPackageKey k) = RuleResult k

-- | Result type of 'Core'
data PackageResult = PackageResult
  { _prname :: PackageName,
    _prversion :: NvcheckerResult,
    _prfetched :: NixFetcher 'Fetched,
    _prpassthru :: Maybe (HashMap Text Text),
    -- | file path relative to package source -> file path relative to build dir
    _prextract :: Maybe (HashMap FilePath FilePath),
    -- | cargo lock file path relative to package source -> (lock file path relative to build dir, git dependencies)
    _prcargolock :: Maybe (HashMap FilePath (FilePath, HashMap Text Checksum)),
    _prpinned :: UseStaleVersion,
    _prgitdate :: Maybe Text
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
        "cargoLock" A..= _prcargolock,
        "pinned" A..= case _prpinned of
          PermanentStale -> True
          _ -> False,
        "date" A..= _prgitdate
      ]
