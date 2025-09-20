{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Copyright: (c) 2021-2025 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module provides function that extracts files contents from package sources.
-- Because we use @nix-instantiate@ to build drv, so @<nixpkgs>@ (@NIX_PATH@) is required.
module NvFetcher.ExtractSrc
  ( -- * Types
    ExtractSrcQ (..),
    Glob (..),

    -- * Rules
    extractSrcRule,

    -- * Functions
    extractSrc,
    extractSrcs,
  )
where

import Control.Monad (filterM, forM, join, void, when)
import Control.Monad.Extra (unlessM)
import Data.Binary.Instances ()
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath (makeRelative, (</>))
import NvFetcher.NixExpr
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils (compileGlob)
import Prettyprinter (pretty, (<+>))
import qualified System.Directory.Extra as IO
import System.FilePath.Glob (globDir1)

-- | Rules of extract source
extractSrcRule :: Rules ()
extractSrcRule = void $
  addOracle $ \q@(ExtractSrcQ fetcher files) -> do
    withTempFile $ \fp -> withRetry $ do
      putInfo . show $ "#" <+> pretty q
      let nixExpr = T.unpack $ fetcherToDrv fetcher "nvfetcher-extract"
      putVerbose $ "Generated nix expr:\n" <> nixExpr
      writeFile' fp nixExpr
      (CmdTime t, StdoutTrim out, CmdLine c, Stdouterr err) <- quietly $ cmd $ "nix-build --no-out-link " <> fp
      putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
      putVerbose $ "Output from stdout: " <> out
      putVerbose $ "Output from stderr: " <> err
      unlessM (liftIO $ IO.doesDirectoryExist out) $
        fail $
          "nix-build output is not a directory: " <> out
      buildDir <- getBuildDir
      HM.fromList
        <$> fmap
          join
          ( sequence
              [ do
                  let compiled = compileGlob glob
                  -- Find all matching files
                  paths <- liftIO $ fmap (makeRelative out) <$> (liftIO (globDir1 compiled out) >>= filterM IO.doesFileExist)
                  putVerbose $ "From glob: " <> coerce glob <> ", found file(s): " <> intercalate ", " paths
                  when (null paths) $ fail $ "No files matched glob: " <> coerce glob
                  forM paths $ \file -> do
                    -- Copy the file to the build directory under the hash of the fetcher
                    -- Also replace slashes in the hash with underscores
                    let dst = (T.unpack . T.replace "/" "_" . coerce $ _sha256 fetcher) </> file
                    copyFile' (out </> file) (buildDir </> dst)
                    pure (file, dst)
                | glob <- NE.toList files
              ]
          )

-- | Run extract source with many sources
extractSrcs ::
  -- | prefetched source
  NixFetcher Fetched ->
  -- | glob patterns
  NE.NonEmpty Glob ->
  Action (HashMap FilePath FilePath)
extractSrcs fetcher xs = askOracle (ExtractSrcQ fetcher xs)

-- | Run extract source
extractSrc ::
  -- | prefetched source
  NixFetcher Fetched ->
  -- | glob pattern
  Glob ->
  Action (HashMap FilePath FilePath)
extractSrc fetcher glob = extractSrcs fetcher $ NE.fromList [glob]
