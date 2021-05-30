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
import Data.Text (Text)
import Data.Void (absurd)
import Development.Shake
import Development.Shake.Rule
import NeatInterpolation (trimming)
import NvFetcher.NixExpr
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.PostFetcher
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils

-- | The core rule of nvchecker.
-- nvchecker rule and prefetch rule are wired here.
coreRules :: Rules ()
coreRules = do
  nvcheckerRule
  prefetchRule
  postFetchRule
  addBuiltinRule noLint noIdentity $ \(WithPackageKey (Core, pkg)) mOld mode -> case mode of
    RunDependenciesSame
      | Just old <- mOld,
        (expr, _) <- decode' @(NixExpr, Version) old ->
        pure $ RunResult ChangedNothing old expr
    _ ->
      lookupPackage pkg >>= \case
        Nothing -> fail $ "Unkown package key: " <> show pkg
        Just Package {..} -> do
          (NvcheckerResult version mOldV) <- checkVersion _pversion pkg
          prefetched <- prefetch $ _pfetcher version
          postfetched <- sequenceA $ (`postfetch` pkg) <$> (($ prefetched) . ($ version) . ($ _pname) <$> _ppostfetch)
          let appending = case postfetched of
                Just (RustLegacy _ _ _ cargoSha256) -> "cargoSha256 = " <> asString (coerce cargoSha256) <> ";"
                Just (Go bot) -> absurd bot
                Nothing -> ""
          case mOldV of
            Nothing ->
              recordVersionChange _pname Nothing version
            Just old
              | old /= version ->
                recordVersionChange _pname (Just old) version
            _ -> pure ()
          let result = gen _pname version prefetched appending
          pure $ RunResult ChangedRecomputeDiff (encode' (result, version)) result

-- | Run the core rule.
-- Given a package key, run nvchecker and then prefetch it,
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
