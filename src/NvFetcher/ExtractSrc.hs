{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
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
  )
where

import Control.Monad (void)
import qualified Data.Aeson as A
import Data.Binary.Instances ()
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import NeatInterpolation (trimming)
import NvFetcher.NixExpr
import NvFetcher.Types

-- | Rules of extract source
extractSrcRule :: Rules ()
extractSrcRule = void $
  addOracleCache $ \(q :: ExtractSrcQ) -> withTempFile $ \fp -> do
    writeFile' fp $ T.unpack $ wrap $ toNixExpr q
    need [fp]
    -- TODO: Avoid using NIX_PATH
    (CmdTime t, StdoutTrim out, CmdLine c) <- cmd Shell $ "nix-instantiate --eval --strict --json --read-write-mode -E 'let pkgs = import <nixpkgs> { }; in ((import " <> fp <> ") pkgs)'"
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case A.decodeStrict out of
      Just x -> pure x
      _ -> fail $ "Failed to parse output of nix-instantiate: " <> T.unpack (T.decodeUtf8 out)

-- | Run extract source
extractSrc ::
  -- | prefetched source
  NixFetcher Fetched ->
  -- | relative file paths to extract
  [FilePath] ->
  Action (HashMap FilePath Text)
extractSrc fetcher fp = askOracle (ExtractSrcQ fetcher fp)

--------------------------------------------------------------------------------

wrap :: NixExpr -> NixExpr
wrap expr =
  [trimming|
    { pkgs, ... }:
    $expr
  |]
