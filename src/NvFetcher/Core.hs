{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Copyright: (c) 2021 berberman
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
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Rule
import NvFetcher.ExtractSrc
import NvFetcher.FetchRustGitDeps
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils

-- | The core rule of nvchecker.
-- all rules are wired here.
coreRules :: Rules ()
coreRules = do
  nvcheckerRule
  prefetchRule
  extractSrcRule
  fetchRustGitDepsRule
  addBuiltinRule noLint noIdentity $ \(WithPackageKey (Core, pkg)) _ _ -> do
    -- it's important to always rerun
    -- since the package definition is not tracked at all
    alwaysRerun
    lookupPackage pkg >>= \case
      Nothing -> fail $ "Unkown package key: " <> show pkg
      Just
        Package
          { _pversion = CheckVersion versionSource options,
            _ppassthru = (PackagePassthru passthru),
            _ppinned = (UseStaleVersion pinned),
            ..
          } -> do
          _prversion@(NvcheckerResult version mOldV _isStale) <- checkVersion versionSource options pkg
          _prfetched <- prefetch $ _pfetcher version
          shakeDir <- getShakeDir
          -- extract src
          _prextract <-
            case _pextract of
              Just (PackageExtractSrc extract) -> do
                result <- HMap.toList <$> extractSrcs _prfetched extract
                Just . HMap.fromList
                  <$> sequence
                    [ do
                        -- write extracted files to shake dir
                        -- and read them in nix using 'builtins.readFile'
                        writeFile' (shakeDir </> path) (T.unpack v)
                        pure (k, T.pack path)
                      | (k, v) <- result,
                        let path =
                              "./"
                                <> T.unpack _pname
                                <> "-"
                                <> T.unpack (coerce version)
                                </> k
                    ]
              _ -> pure Nothing
          -- cargo lock
          (_prcargolock, _prrustgitdeps) <-
            case _pcargo of
              Just (PackageCargoFilePath lockPath) -> do
                (_, lockData) <- head . HMap.toList <$> extractSrc _prfetched lockPath
                result <- fetchRustGitDeps _prfetched lockPath
                let lockPath' =
                      T.unpack _pname
                        <> "-"
                        <> T.unpack (coerce version)
                        </> lockPath
                    lockPathNix = "./" <> T.pack lockPath'
                -- similar to extract src, write lock file to shake dir
                writeFile' (shakeDir </> lockPath') $ T.unpack lockData
                pure (Just lockPathNix, Just result)
              _ -> pure (Nothing, Nothing)

          -- update changelog
          case mOldV of
            Nothing ->
              recordVersionChange _pname Nothing version
            Just old
              | old /= version ->
                recordVersionChange _pname (Just old) version
            _ -> pure ()

          let _prpassthru = if HMap.null passthru then Nothing else Just passthru
              _prname = _pname
              _prpinned = pinned
          -- Since we don't save the previous result, we are not able to know if the result changes
          -- Depending on this rule leads to RunDependenciesChanged
          pure $ RunResult ChangedRecomputeDiff mempty PackageResult {..}

-- | 'Core' rule take a 'PackageKey', find the corresponding 'Package', and run all needed rules to get 'PackageResult'
runPackage :: PackageKey -> Action PackageResult
runPackage k = apply1 $ WithPackageKey (Core, k)
