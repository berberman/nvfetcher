{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Copyright: (c) 2021-2022 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module provides function that extracs files contents from package sources.
-- It uses [IFD](https://nixos.wiki/wiki/Import_From_Derivation) under the hood,
-- pulling /textual/ files from source drv.
-- Because we use @nix-instantiate@ to build drv, so @<nixpkgs>@ (@NIX_PATH@) is required.
module NvFetcher.ExtractSrc
  ( -- * Types
    ExtractSrcQ (..),

    -- * Rules
    extractSrcRule,
    extractSrc,
    extractSrcs,
  )
where

import Control.Monad (void)
import Control.Monad.Extra (unlessM)
import Data.Binary.Instances ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath ((</>))
import NvFetcher.NixExpr
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import Prettyprinter (pretty, (<+>))

-- | Rules of extract source
extractSrcRule :: Rules ()
extractSrcRule = void $
  addOracle $ \q@(ExtractSrcQ fetcher files) -> withTempFile $ \fp -> withRetry $ do
    putInfo . show $ "#" <+> pretty q
    let nixExpr = T.unpack $ fetcherToDrv fetcher "nvfetcher-extract"
    putVerbose $ "Generated nix expr:\n" <> nixExpr
    writeFile' fp nixExpr
    (CmdTime t, StdoutTrim out, CmdLine c) <- quietly $ cmd $ "nix-build " <> fp
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    unlessM (doesDirectoryExist out) $
      fail $ "nix-build output is not a directory: " <> out
    HM.fromList <$> sequence [(f,) <$> liftIO (T.readFile $ out </> f) | f <- NE.toList files]

-- | Run extract source with many sources
extractSrcs ::
  -- | prefetched source
  NixFetcher Fetched ->
  -- | relative file paths to extract
  NE.NonEmpty FilePath ->
  Action (HashMap FilePath Text)
extractSrcs fetcher xs = askOracle (ExtractSrcQ fetcher xs)

-- | Run extract source
extractSrc ::
  -- | prefetched source
  NixFetcher Fetched ->
  -- | relative file path to extract
  FilePath ->
  Action (HashMap FilePath Text)
extractSrc fetcher fp = extractSrcs fetcher $ NE.fromList [fp]
