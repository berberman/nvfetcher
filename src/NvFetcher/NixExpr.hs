{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module contains a type class 'ToNixExpr' and some its instances associated with either Haskell
-- primitive types or our "NvFetcher.Types".
module NvFetcher.NixExpr
  ( NixExpr,
    ToNixExpr (..),
  )
where

import Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import NvFetcher.Types
import NvFetcher.Utils (asString)

-- | Types can be converted into nix expr
class ToNixExpr a where
  toNixExpr :: a -> NixExpr

instance ToNixExpr (NixFetcher Fresh) where
  toNixExpr = nixFetcher "lib.fakeSha256"

instance ToNixExpr (NixFetcher Fetched) where
  toNixExpr f = nixFetcher (asString $ coerce $ _sha256 f) f

instance ToNixExpr Bool where
  toNixExpr True = "true"
  toNixExpr False = "false"

instance ToNixExpr a => ToNixExpr [a] where
  toNixExpr xs = foldl (\acc x -> acc <> " " <> toNixExpr x) "[" xs <> " ]"

instance ToNixExpr a => ToNixExpr (NE.NonEmpty a) where
  toNixExpr = toNixExpr . NE.toList

instance {-# OVERLAPS #-} ToNixExpr String where
  toNixExpr = T.pack . show

instance ToNixExpr NixExpr where
  toNixExpr = id

instance ToNixExpr Version where
  toNixExpr = coerce

nixFetcher :: Text -> NixFetcher k -> NixExpr
nixFetcher sha256 = \case
  FetchGit
    { _sha256 = _,
      _rev = asString . toNixExpr -> rev,
      _fetchSubmodules = toNixExpr -> fetchSubmodules,
      _deepClone = toNixExpr -> deepClone,
      _leaveDotGit = toNixExpr -> leaveDotGit,
      _furl = asString -> url
    } ->
      [trimming|
          fetchgit {
            url = $url;
            rev = $rev;
            fetchSubmodules = $fetchSubmodules;
            deepClone = $deepClone;
            leaveDotGit = $leaveDotGit;
            sha256 = $sha256;
          }
    |]
  (FetchUrl (asString -> url) _) ->
    [trimming|
          fetchurl {
            url = $url;
            sha256 = $sha256;
          }
    |]

instance ToNixExpr ExtractSrcQ where
  toNixExpr (ExtractSrcQ fetcher files) = extractFiles fetcher files

extractFiles :: NixFetcher Fetched -> NE.NonEmpty FilePath -> NixExpr
extractFiles (toNixExpr -> fetcherExpr) (toNixExpr -> fileNames) =
  [trimming|
    let
      drv = import (pkgs.writeText "src" ''
        pkgs: {
          src = pkgs.$fetcherExpr;
        }
      '');
      fileNames = $fileNames;
      toFile = f: builtins.readFile ((drv pkgs).src + "/" + f);
    in builtins.listToAttrs (builtins.map (x: {
      name = x;
      value = toFile x;
    }) fileNames)
  |]
