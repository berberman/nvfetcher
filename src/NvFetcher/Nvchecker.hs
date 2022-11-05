{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2021-2022 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- [nvchecker](https://github.com/lilydjwg/nvchecker) is a program checking new versions of packages.
-- We encode the checking process into shake build system, generating configuration of nvchecker and calling it externally.
-- Now we call nvchecker for each 'VersionSource', which seems not to be efficient, but it's tolerable when running in parallel.
--
-- Meanwhile, we lose the capabilities of tracking version updates, i.e. normally nvchecker will help us maintain a list of old versions,
-- so that we are able to know which package's version is updated in this run. Fortunately, we can reimplement this in shake,
-- see 'nvcheckerRule' for details.
module NvFetcher.Nvchecker
  ( -- * Types
    VersionSortMethod (..),
    ListOptions (..),
    CheckVersion (..),
    NvcheckerOptions (..),
    VersionSource (..),
    NvcheckerResult (..),

    -- * Rules
    nvcheckerRule,

    -- * Functions
    checkVersion,
    checkVersion',
  )
where

import Control.Monad (void)
import Control.Monad.Extra (fromMaybeM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Rule
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils
import Prettyprinter (pretty, (<+>))
import Toml (Value (Bool, Text))
import qualified Toml
import Toml.Type.Edsl

-- | Rules of nvchecker
nvcheckerRule :: Rules ()
nvcheckerRule = do
  persistedRule
  oneShotRule

-- | Nvchecker rule for packages, which is aware of version changes and supports using stale version.
-- nvchecker will be called at most one time given a package key. Follow-up using of this rule will return cached result.
-- 'PackageKey' is required for caching.
-- Run this rule by calling 'checkVersion'
persistedRule :: Rules ()
persistedRule = addBuiltinRule noLint noIdentity $ \(WithPackageKey (key@(CheckVersion versionSource options), pkg)) _old _mode -> do
  putInfo . show $ "#" <+> pretty key
  oldVer <- getRecentLastVersion pkg
  useStaleVersion <- _ppinned . fromJust <$> lookupPackage pkg
  let useStale = case useStaleVersion of
        PermanentStale -> True
        TemporaryStale -> True
        _ -> False
  case useStale of
    True
      | Just oldVer' <- oldVer -> do
          -- use the stale version if we have
          putInfo $ T.unpack $ "Skip running nvchecker, use stale version " <> coerce oldVer' <> " for " <> coerce pkg
          let result = NvcheckerResult {nvNow = oldVer', nvOld = oldVer, nvStale = True}
          pure $ RunResult ChangedRecomputeSame (encode' result) result

    -- run nvchecker
    _ -> do
      -- if we already run this rule for a package, we can recover the last result from getLastVersionUpdated
      -- (when cacheNvchecker is enabled)
      useCache <- nvcheckerCacheEnabled
      now <- fromMaybeM (coerce <$> runNvchecker pkg options versionSource) (if useCache then getLastVersionUpdated pkg else pure Nothing)
      let runChanged = case oldVer of
            Just oldVer'
              | oldVer' == now -> ChangedRecomputeSame
            _ -> ChangedRecomputeDiff
          result = NvcheckerResult {nvNow = now, nvOld = oldVer, nvStale = False}
      -- always update
      updateLastVersion pkg now
      pure $ RunResult runChanged mempty result

-- | Nvchecker rule without cache
-- Rule this rule by calling 'checkVersion''
oneShotRule :: Rules ()
oneShotRule = void $
  addOracle $ \key@(CheckVersion versionSource options) -> do
    putInfo . show $ pretty key
    now <- runNvchecker (PackageKey "pkg") options versionSource
    pure $ NvcheckerResult now Nothing False

runNvchecker :: PackageKey -> NvcheckerOptions -> VersionSource -> Action Version
runNvchecker pkg options versionSource = withTempFile $ \config -> withRetry $ do
  mKeyfile <- getKeyfilePath
  let nvcheckerConfig = T.unpack $ Toml.pretty $ mkToml $ genNvConfig pkg options mKeyfile versionSource
  putVerbose $ "Generated nvchecker config for " <> show pkg <> ":" <> nvcheckerConfig
  writeFile' config nvcheckerConfig
  (CmdTime t, Stdout out, CmdLine c) <- quietly . cmd $ "nvchecker --logger json -c " <> config
  putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
  case reverse . lines $ out of
    [o] | Just raw <- A.decodeStrict' $ BS.pack o -> case raw of
      NvcheckerSuccess x -> pure x
      NvcheckerError err -> fail $ "Failed to run nvchecker: " <> T.unpack err
    _ -> fail $ "Failed to parse output from nvchecker: " <> out

genNvConfig :: PackageKey -> NvcheckerOptions -> Maybe FilePath -> VersionSource -> TDSL
genNvConfig pkg options mKeyfile versionSource =
  case mKeyfile of
    Just keyfile -> do
      table "__config__" $
        "keyfile" =: Text (T.pack keyfile)
    _ -> pure ()
    >> table
      (fromString $ T.unpack $ coerce pkg)
      ( do
          genVersionSource versionSource
          genOptions options
      )
  where
    key =:? (Just x) = key =: Text x
    _ =:? _ = pure ()
    genVersionSource = \case
      GitHubRelease {..} -> do
        "source" =: "github"
        "github" =: Text (_owner <> "/" <> _repo)
        "use_latest_release" =: Bool True
      GitHubTag {..} -> do
        "source" =: "github"
        "github" =: Text (_owner <> "/" <> _repo)
        "use_max_tag" =: Bool True
        genListOptions _listOptions
      Git {..} -> do
        "source" =: "git"
        "git" =: Text _vurl
        "branch" =:? coerce _vbranch
        "use_commit" =: Bool True
      Aur {..} -> do
        "source" =: "aur"
        "aur" =: Text _aur
        "strip_release" =: Bool True
      ArchLinux {..} -> do
        "source" =: "archpkg"
        "archpkg" =: Text _archpkg
        "strip_release" =: Bool True
      Pypi {..} -> do
        "source" =: "pypi"
        "pypi" =: Text _pypi
      Manual {..} -> do
        "source" =: "manual"
        "manual" =: Text _manual
      Repology {..} -> do
        "source" =: "repology"
        "repology" =: Text _repology
        "repo" =: Text _repo
      Webpage {..} -> do
        "source" =: "regex"
        "url" =: Text _vurl
        "regex" =: Text _regex
        genListOptions _listOptions
      HttpHeader {..} -> do
        "source" =: "httpheader"
        "url" =: Text _vurl
        "regex" =: Text _regex
        genListOptions _listOptions
      OpenVsx {..} -> do
        "source" =: "openvsx"
        "openvsx" =: Text (_ovPublisher <> "." <> _ovExtName)
      VscodeMarketplace {..} -> do
        "source" =: "vsmarketplace"
        "vsmarketplace" =: Text (_vsmPublisher <> "." <> _vsmExtName)
      Cmd {..} -> do
        "source" =: "cmd"
        "cmd" =: Text _vcmd
      Container {..} -> do
        "source" =: "container"
        "container" =: Text _vcontainer
        genListOptions _listOptions
    genListOptions ListOptions {..} = do
      "include_regex" =:? _includeRegex
      "exclude_regex" =:? _excludeRegex
      "sort_version_key" =:? fmap (T.pack . show) _sortVersionKey
      "ignored" =:? _ignored
    genOptions NvcheckerOptions {..} = do
      "prefix" =:? _stripPrefix
      "from_pattern" =:? _fromPattern
      "to_pattern" =:? _toPattern

-- | Run nvchecker given 'PackageKey'
-- Recording version changes and using stale version are available.
checkVersion :: VersionSource -> NvcheckerOptions -> PackageKey -> Action NvcheckerResult
checkVersion v o k = apply1 $ WithPackageKey (CheckVersion v o, k)

-- | Run nvchecker without cache
checkVersion' :: VersionSource -> NvcheckerOptions -> Action NvcheckerResult
checkVersion' v o = askOracle $ CheckVersion v o
