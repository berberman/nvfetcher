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
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import NvFetcher.Types
import NvFetcher.Utils (quote, quoteIfNeeds)

-- | Types can be converted into nix expr
class ToNixExpr a where
  toNixExpr :: a -> NixExpr

instance ToNixExpr (NixFetcher Fresh) where
  toNixExpr = nixFetcher "lib.fakeSha256"

instance ToNixExpr (NixFetcher Fetched) where
  toNixExpr f = nixFetcher (quote $ coerce $ _sha256 f) f

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
      _rev = quote . toNixExpr -> rev,
      _fetchSubmodules = toNixExpr -> fetchSubmodules,
      _deepClone = toNixExpr -> deepClone,
      _leaveDotGit = toNixExpr -> leaveDotGit,
      _furl = quote -> url,
      _name = nameField -> n
    } ->
      [trimming|
          fetchgit {
            url = $url;
            rev = $rev;
            fetchSubmodules = $fetchSubmodules;
            deepClone = $deepClone;
            leaveDotGit = $leaveDotGit;$n
            sha256 = $sha256;
          }
    |]
  FetchGitHub
    { _sha256 = _,
      _rev = quote . toNixExpr -> rev,
      _fetchSubmodules = toNixExpr -> fetchSubmodules,
      _deepClone = toNixExpr -> deepClone,
      _leaveDotGit = toNixExpr -> leaveDotGit,
      _fowner = quote -> owner,
      _frepo = quote -> repo,
      _name = nameField -> n
    } ->
      -- TODO: fix fetchFromGitHub in Nixpkgs so that deepClone and
      -- leaveDotGit won't get passed to fetchzip
      if (deepClone == "true") || (leaveDotGit == "true")
        then
          [trimming|
               fetchFromGitHub ({
                 owner = $owner;
                 repo = $repo;
                 rev = $rev;
                 fetchSubmodules = $fetchSubmodules;
                 deepClone = $deepClone;
                 leaveDotGit = $leaveDotGit;$n
                 sha256 = $sha256;
               })
         |]
        else
          [trimming|
               fetchFromGitHub ({
                 owner = $owner;
                 repo = $repo;
                 rev = $rev;
                 fetchSubmodules = $fetchSubmodules;$n
                 sha256 = $sha256;
               })
         |]
  (FetchUrl (quote -> url) (nameField -> n) _) ->
    [trimming|
          fetchurl {
            url = $url;$n
            sha256 = $sha256;
          }
    |]
  (FetchTarball (quote -> url) _) ->
    [trimming|
          fetchTarball {
            url = $url;
            sha256 = $sha256;
          }
    |]
  where
    nameField = maybe "" (\x -> "\nname = " <> quote x <> ";")

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

-- | nix expr snippet like:
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
instance ToNixExpr PackageResult where
  toNixExpr PackageResult {..} =
    [trimming|
        $name = {
          pname = $nameString;
          version = $version;
          src = $src;$appending
        };
    |]
    where
      name = quoteIfNeeds _prname
      nameString = quote _prname
      version = quote . coerce . nvNow $ _prversion
      src = toNixExpr _prfetched
      extract =
        maybe
          ""
          ( \ex ->
              T.unlines
                [ quoteIfNeeds (T.pack name)
                    <> " = builtins.readFile "
                    <> fp
                    <> ";"
                  | (name, fp) <- HMap.toList ex
                ]
          )
          _prextract
      cargo = fromMaybe "" $ do
        fp <- _prcargolock
        deps <-
          ( \deps ->
              T.unlines
                [ quoteIfNeeds name
                    <> " = "
                    <> quote (coerce sum)
                    <> ";"
                  | (name, sum) <- HMap.toList deps
                ]
            )
            <$> _prrustgitdeps
        pure
          [trimming|
            cargoLock = {
              lockFile = $fp;
              outputHashes = {
                $deps
              };
            };
          |]
      passthru =
        maybe
          ""
          ( \pt ->
              T.unlines
                [ quoteIfNeeds k
                    <> " = "
                    <> v
                    <> ";"
                  | (k, quote -> v) <- HMap.toList pt
                ]
          )
          _prpassthru
      joined = extract <> cargo <> passthru
      appending = if T.null joined then "" else "\n" <> joined
