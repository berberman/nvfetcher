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

import Control.Monad (void)
import Data.Coerce (coerce)
import Development.Shake
import NeatInterpolation (trimming)
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.ShakeExtras
import NvFetcher.Types

coreRules :: Rules ()
coreRules = do
  void $ addOracle run
  nvcheckerRule
  prefetchRule
  where
    run key =
      getPackageByKey key >>= \case
        Nothing -> fail $ "Unkown package key: " <> show key
        Just Package {..} -> do
          (NvcheckerResult version mOld) <- checkVersion pversion key
          prefetched <- prefetch $ pfetcher version
          case mOld of
            Nothing ->
              recordVersionChange pname Nothing version
            Just old
              | old /= version ->
                recordVersionChange pname (Just old) version
            _ -> pure ()
          pure $ gen (pname, version, prefetched)
    gen (name, coerce @Version -> ver, toNixExpr -> srcP) =
      [trimming|
      $name = {
        pname = "$name";
        version = "$ver";
        src = $srcP;
      };
    |]

generateNixSourceExpr :: PackageKey -> Action NixExpr
generateNixSourceExpr = askOracle
