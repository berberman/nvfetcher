{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
  ( coreRules,
    generateNixSourceExpr,
  )
where

import Data.Coerce (coerce)
import Data.Text (Text)
import Development.Shake
import Development.Shake.Rule
import NeatInterpolation (trimming)
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.ShakeExtras
import NvFetcher.Types
import NvFetcher.Utils

-- | The core rule of nvchecker.
-- nvchecker rule and prefetch rule are wired here.
coreRules :: Rules ()
coreRules = do
  nvcheckerRule
  prefetchRule
  addBuiltinRule noLint noIdentity $ \(WithPackageKey (Core, pkg)) mOld mode -> case mode of
    RunDependenciesSame
      | Just old <- mOld,
        (expr, _) <- decode' @(NixExpr, Version) old ->
        pure $ RunResult ChangedNothing old expr
    _ ->
      lookupPackage pkg >>= \case
        Nothing -> fail $ "Unkown package key: " <> show pkg
        Just Package {..} -> do
          (NvcheckerResult version mOldV) <- checkVersion pversion pkg
          prefetched <- prefetch $ pfetcher version
          case mOldV of
            Nothing ->
              recordVersionChange pname Nothing version
            Just old
              | old /= version ->
                recordVersionChange pname (Just old) version
            _ -> pure ()
          let result = gen pname version prefetched
          pure $ RunResult ChangedRecomputeDiff (encode' (result, version)) result

-- | Run the core rule.
-- Given a package key, run nvchecker and then prefetch it,
-- resulting a nix source snippet like:
--
-- @@
-- feeluown-core = {
--     pname = "feeluown-core";
--     version = "3.7.7";
--     src = fetchurl {
--       sha256 = "06d3j39ff9znqxkhp9ly81lcgajkhg30hyqxy2809yn23xixg3x2";
--       url = "https://pypi.io/packages/source/f/feeluown/feeluown-3.7.7.tar.gz";
--     };
--   };
-- @@
generateNixSourceExpr :: PackageKey -> Action NixExpr
generateNixSourceExpr k = apply1 $ WithPackageKey (Core, k)

gen :: PackageName -> Version -> NixFetcher Prefetched -> Text
gen name (coerce -> ver) (toNixExpr -> srcP) =
  [trimming|
  $name = {
    pname = "$name";
    version = "$ver";
    src = $srcP;
  };
|]
