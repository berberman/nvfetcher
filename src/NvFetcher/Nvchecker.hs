{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
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
-- so that we are able to know which package's version is updated in this run. Fortunately, we can reimplement this using shake database,
-- see 'nvcheckerRule' for details.
module NvFetcher.Nvchecker
  ( -- * Types
    VersionSortMethod (..),
    ListOptions (..),
    NvcheckerQ (..),
    NvcheckerOptions (..),
    VersionSource (..),
    NvcheckerA (..),

    -- * Rules
    nvcheckerRule,
    checkVersion,
  )
where

import qualified Data.Aeson as A
import Data.Coerce (coerce)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Development.Shake.Rule
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils
import Toml (Value (Bool, Text), pretty)
import Toml.Type.Edsl

-- | Rules of nvchecker
nvcheckerRule :: Rules ()
nvcheckerRule = addBuiltinRule noLint noIdentity $ \(WithPackageKey (NvcheckerQ versionSource options, pkg)) old _mode ->
  -- If the package was removed after the last run,
  -- shake still runs the nvchecker rule for this package.
  -- So we record a version change here, indicating that the package has been removed.
  -- Ideally, this should be done in the core rule
  isPackageKeyTarget pkg >>= \case
    False -> do
      let oldVer = decode' <$> old
      recordVersionChange (coerce pkg) oldVer "∅"
      pure $ RunResult ChangedRecomputeDiff mempty undefined -- skip running, returning a never consumed result
    _ ->
      withTempFile $ \config -> withRetries $ do
        writeFile' config $ T.unpack $ pretty $ mkToml $ genNvConfig pkg options versionSource
        need [config]
        (CmdTime t, Stdout out, CmdLine c) <- cmd $ "nvchecker --logger json -c " <> config
        putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
        let out' = T.decodeUtf8 out
            result = mapMaybe (A.decodeStrict . T.encodeUtf8) (T.lines out')
        now <- case result of
          [x] -> pure x
          _ -> fail $ "Failed to parse output from nvchecker: " <> T.unpack out'
        pure $ case old of
          Just lastRun
            | cachedResult <- decode' lastRun ->
              if cachedResult == nvNow now
                then -- try to get the version in last run from store, filling it into 'now'
                  RunResult ChangedRecomputeSame lastRun now {nvOld = Just cachedResult}
                else RunResult ChangedRecomputeDiff (encode' $ nvNow now) now {nvOld = Just cachedResult}
          Nothing -> RunResult ChangedRecomputeDiff (encode' $ nvNow now) now

genNvConfig :: PackageKey -> NvcheckerOptions -> VersionSource -> TDSL
genNvConfig pkg options versionSource = table (fromString $ T.unpack $ coerce pkg) $ do
  genVersionSource versionSource
  genOptions options
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
    genListOptions ListOptions {..} = do
      "include_regex" =:? _includeRegex
      "exclude_regex" =:? _excludeRegex
      "sort_version_key" =:? fmap (T.pack . show) _sortVersionKey
      "ignored" =:? _ignored
    genOptions NvcheckerOptions {..} = do
      "prefix" =:? _stripPrefix
      "from_pattern" =:? _fromPattern
      "to_pattern" =:? _toPattern

-- | Run nvchecker
checkVersion :: VersionSource -> NvcheckerOptions -> PackageKey -> Action NvcheckerA
checkVersion v o k = apply1 $ WithPackageKey (NvcheckerQ v o, k)
