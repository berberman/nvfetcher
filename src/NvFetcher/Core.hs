{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
module NvFetcher.Core
  ( Core (..),
    coreRules,
    generateNixSourceExpr,
  )
where

import Control.Monad (void)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import NeatInterpolation (trimming)
import NvFetcher.ExtractSrc
import NvFetcher.FetchRustGitDeps
import NvFetcher.NixExpr
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
  void $
    addOracle $ \(WithPackageKey (Core, pkg)) -> do
      -- it's important to always rerun
      -- since the package definition is not tracked at all
      alwaysRerun
      lookupPackage pkg >>= \case
        Nothing -> fail $ "Unkown package key: " <> show pkg
        Just
          Package
            { _pversion = CheckVersion versionSource options,
              _ppassthru = PackagePassthru passthruMap,
              ..
            } -> do
            (NvcheckerResult version mOldV _isStale) <- checkVersion versionSource options pkg
            prefetched <- prefetch $ _pfetcher version
            shakeDir <- getShakeDir
            -- extract src
            appending1 <-
              case _pextract of
                Just (PackageExtractSrc extract) -> do
                  result <- HMap.toList <$> extractSrcs prefetched extract
                  T.unlines
                    <$> sequence
                      [ do
                          -- write extracted files to shake dir
                          -- and read them in nix using 'builtins.readFile'
                          writeFile' (shakeDir </> path) (T.unpack v)
                          pure $ toNixExpr k <> " = builtins.readFile ./" <> T.pack path <> ";"
                        | (k, v) <- result,
                          let path =
                                T.unpack _pname
                                  <> "-"
                                  <> T.unpack (coerce version)
                                  </> k
                      ]
                _ -> pure ""
            -- cargo lock
            appending2 <-
              case _pcargo of
                Just (PackageCargoFilePath lockPath) -> do
                  (_, lockData) <- head . HMap.toList <$> extractSrc prefetched lockPath
                  result <- fetchRustGitDeps prefetched lockPath
                  let body = T.unlines [quote k <> " = " <> coerce (quote $ coerce v) <> ";" | (k, v) <- HMap.toList result]
                      lockPath' =
                        T.unpack _pname
                          <> "-"
                          <> T.unpack (coerce version)
                          </> lockPath
                      lockPathNix = "./" <> T.pack lockPath'
                  -- similar to extract src, write lock file to shake dir
                  writeFile' (shakeDir </> lockPath') $ T.unpack lockData
                  pure
                    [trimming|
                  cargoLock = {
                    lockFile = $lockPathNix;
                    outputHashes = {
                      $body
                    };
                  };
                |]
                _ -> pure ""
            -- passthru
            let appending3 = T.unlines [k <> " = " <> v <> ";" | (k, quote -> v) <- HMap.toList passthruMap]

            -- update changelog
            case mOldV of
              Nothing ->
                recordVersionChange _pname Nothing version
              Just old
                | old /= version ->
                  recordVersionChange _pname (Just old) version
              _ -> pure ()

            pure $ gen _pname version prefetched $ appending1 <> appending2 <> appending3

-- | Run the core rule.
-- Given a 'PackageKey', run "NvFetcher.Nvchecker", "NvFetcher.NixFetcher"
-- (may also run "NvFetcher.ExtractSrc" or "NvFetrcher.FetchRustGitDeps")
--
-- @
-- Package
-- { _pname = "feeluown-core",
--   _pversion = NvcheckerQ (Pypi "feeluown") def,
--   _pfetcher = pypiFetcher "feeluown",
--   _pextract = Nothing,
--   _pcargo = Nothing,
--   _ppassthru = PackagePassthru (HashMap.fromList [("a", "B")])
-- }
-- @
--
-- resulting a nix exprs snippet like:
--
-- @
-- feeluown-core = {
--     pname = "feeluown-core";
--     version = "3.7.7";
--     src = fetchurl {
--       sha256 = "06d3j39ff9znqxkhp9ly81lcgajkhg30hyqxy2809yn23xixg3x2";
--       url = "https://pypi.io/packages/source/f/feeluown/feeluown-3.7.7.tar.gz";
--     };
--     a = "B";
--   };
-- @
generateNixSourceExpr :: PackageKey -> Action NixExpr
generateNixSourceExpr k = askOracle $ WithPackageKey (Core, k)

gen :: PackageName -> Version -> NixFetcher Fetched -> Text -> Text
gen name (coerce -> ver) (toNixExpr -> srcP) appending =
  [trimming|
  $name = {
    pname = "$name";
    version = "$ver";
    src = $srcP;$appending'
  };
|]
  where
    appending' = if T.null appending then "" else "\n" <> appending
