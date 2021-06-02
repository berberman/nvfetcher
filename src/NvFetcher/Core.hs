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

import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Rule
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
  addBuiltinRule noLint noIdentity $ \(WithPackageKey (Core, pkg)) mOld mode -> case mode of
    RunDependenciesSame
      | Just old <- mOld,
        (expr, _) <- decode' @(NixExpr, Version) old ->
        pure $ RunResult ChangedNothing old expr
    _ ->
      lookupPackage pkg >>= \case
        Nothing -> fail $ "Unkown package key: " <> show pkg
        Just
          Package
            { _pversion = NvcheckerQ versionSource options,
              _pextract = PackageExtractSrc extract,
              ..
            } -> do
            (NvcheckerA version mOldV) <- checkVersion versionSource options pkg
            prefetched <- prefetch $ _pfetcher version
            appending1 <-
              if null extract
                then pure ""
                else do
                  result <- HMap.toList <$> extractSrc prefetched extract
                  pure $ T.unlines [toNixExpr k <> " = ''\n" <> v <> "'';" | (k, v) <- result]
            appending2 <-
              case _pcargo of
                Just (PackageCargoFilePath lockPath) -> do
                  result <- fetchRustGitDeps prefetched lockPath
                  let body = T.unlines [asString k <> " = " <> coerce (asString $ coerce v) <> ";" | (k, v) <- HMap.toList result]
                      lockPathNix = T.pack $ "./" <> lockPath
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
            case mOldV of
              Nothing ->
                recordVersionChange _pname Nothing version
              Just old
                | old /= version ->
                  recordVersionChange _pname (Just old) version
              _ -> pure ()
            let result = gen _pname version prefetched $ appending1 <> appending2
            pure $ RunResult ChangedRecomputeDiff (encode' (result, version)) result

-- | Run the core rule.
-- Given a 'PackageKey', run "NvFetcher.Nvchecker", "NvFetcher.NixFetcher"
-- (may also run "NvFetcher.ExtractSrc" or "FetchRustGitDeps")
-- resulting a nix source snippet like:
--
-- @
-- feeluown-core = {
--     pname = "feeluown-core";
--     version = "3.7.7";
--     src = fetchurl {
--       sha256 = "06d3j39ff9znqxkhp9ly81lcgajkhg30hyqxy2809yn23xixg3x2";
--       url = "https://pypi.io/packages/source/f/feeluown/feeluown-3.7.7.tar.gz";
--     };
--   };
-- @
generateNixSourceExpr :: PackageKey -> Action NixExpr
generateNixSourceExpr k = apply1 $ WithPackageKey (Core, k)

gen :: PackageName -> Version -> NixFetcher Fetched -> Text -> Text
gen name (coerce -> ver) (toNixExpr -> srcP) appending =
  [trimming|
  $name = {
    pname = "$name";
    version = "$ver";
    src = $srcP;
    $appending
  };
|]
