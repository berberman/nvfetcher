{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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
      getPackageByKey pkg >>= \case
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
