{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2021-2025 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
module NvFetcher.Core
  ( Core (..),
    coreRules,
    runPackage,
  )
where

import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HMap
import Development.Shake
import Development.Shake.Rule
import NvFetcher.ExtractSrc
import NvFetcher.FetchRustGitDeps
import NvFetcher.GetGitCommitDate
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras

-- | The core rule of nvchecker.
-- all rules are wired here.
coreRules :: Rules ()
coreRules = do
  nvcheckerRule
  prefetchRule
  extractSrcRule
  fetchRustGitDepsRule
  getGitCommitDateRule
  addBuiltinRule noLint noIdentity $ \(WithPackageKey (Core, pkg)) _ _ -> do
    -- It's important to always rerun since the package definition is not tracked at all
    -- Also we generate new files in the build directory
    alwaysRerun
    lookupPackage pkg >>= \case
      Nothing -> fail $ "Unknown package key: " <> show pkg
      Just
        Package
          { _pversion = CheckVersion versionSource options,
            _ppassthru = (PackagePassthru passthru),
            ..
          } -> do
          _prversion@(NvcheckerResult version _mOldV _isStale) <- checkVersion versionSource options pkg
          _prfetched <- prefetch (_pfetcher version) _pforcefetch
          -- If we fail to prefetch, we should fail on this package
          case _prfetched of
            Just _prfetched -> do
              -- extract src
              _prextract <-
                case _pextract of
                  Just (PackageExtractSrc extract) -> do
                    result <- extractSrcs _prfetched extract
                    pure $ Just result
                  _ -> pure Nothing
              -- cargo locks
              _prcargolock <-
                case _pcargo of
                  Just (PackageCargoLockFiles lockPath) -> do
                    lockFiles <- HMap.toList <$> extractSrcs _prfetched lockPath
                    result <- parallel $
                      flip fmap lockFiles $ \(srcLockPath, dstLockPath) -> do
                        result <- fetchRustGitDeps _prfetched srcLockPath
                        pure (srcLockPath, (dstLockPath, result))
                    pure . Just $ HMap.fromList result
                  _ -> pure Nothing

              -- Only git version source supports git commit date
              _prgitdate <- case versionSource of
                Git {..} -> Just <$> getGitCommitDate _vurl (coerce version) _pgitdateformat
                _ -> pure Nothing

              -- update changelog
              -- always use on disk version
              mOldV <- getLastVersionOnDisk pkg
              case mOldV of
                Nothing ->
                  recordVersionChange _pname Nothing version
                Just old
                  | old /= version ->
                      recordVersionChange _pname (Just old) version
                _ -> pure ()

              let _prpassthru = if HMap.null passthru then Nothing else Just passthru
                  _prname = _pname
                  _prpinned = _ppinned
              -- Since we don't save the previous result, we are not able to know if the result changes
              -- Depending on this rule leads to RunDependenciesChanged
              pure $ RunResult ChangedRecomputeDiff mempty $ Just PackageResult {..}
            _ -> pure $ RunResult ChangedRecomputeDiff mempty Nothing

-- | 'Core' rule take a 'PackageKey', find the corresponding 'Package', and run all needed rules to get 'PackageResult'
runPackage :: PackageKey -> Action (Maybe PackageResult)
runPackage k = apply1 $ WithPackageKey (Core, k)
