{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
    VersionSource (..),
    NvcheckerResult (..),

    -- * Rules
    nvcheckerRule,
    checkVersion,
  )
where

import qualified Data.Aeson as A
import Data.Coerce (coerce)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Development.Shake.Rule
import NeatInterpolation (trimming)
import NvFetcher.ShakeExtras
import NvFetcher.Types
import NvFetcher.Utils

-- | Rules of nvchecker
nvcheckerRule :: Rules ()
nvcheckerRule = addBuiltinRule noLint noIdentity $ \(WithPackageKey (q, pkg)) old _mode ->
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
      withTempFile $ \config -> do
        writeFile' config $ T.unpack $ genNvConfig "pkg" q
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

genNvConfig :: Text -> VersionSource -> Text
genNvConfig srcName = \case
  GitHubRelease {..} ->
    [trimming|
          [$srcName]
          source = "github"
          github = "$_owner/$_repo"
          use_latest_release = true
    |]
  Git {..} ->
    [trimming|
          [$srcName]
          source = "git"
          git = "$_vurl"
          use_commit = true
    |]
  Aur {..} ->
    [trimming|
          [$srcName]
          source = "aur"
          aur = "$_aur"
          strip_release = true
    |]
  ArchLinux {..} ->
    [trimming|
          [$srcName]
          source = "archpkg"
          archpkg = "$_archpkg"
          strip_release = true
    |]
  Pypi {..} ->
    [trimming|
          [$srcName]
          source = "pypi"
          pypi = "$_pypi"
    |]
  Manual {..} ->
    [trimming|
          [$srcName]
          source = "manual"
          manual = "$_manual"
    |]
  Repology {..} ->
    [trimming|
          [$srcName]
          source = "repology"
          repology = "$_repology"
          repo = "$_repo"
    |]

-- | Run nvchecker
checkVersion :: VersionSource -> PackageKey -> Action NvcheckerResult
checkVersion v k = apply1 $ WithPackageKey (v, k)
