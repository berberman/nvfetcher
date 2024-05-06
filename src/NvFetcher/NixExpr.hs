{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021-2022 berberman
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
    fetcherToDrv,
  )
where

import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
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

instance ToNixExpr (NixFetcher Fetched) where
  toNixExpr = nixFetcher

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

nixFetcher :: NixFetcher Fetched -> NixExpr
nixFetcher = \case
  FetchGit
    { _sha256 = coerce quote -> sha256,
      _rev = quote . toNixExpr -> rev,
      _fetchSubmodules = toNixExpr -> fetchSubmodules,
      _deepClone = toNixExpr -> deepClone,
      _leaveDotGit = toNixExpr -> leaveDotGit,
      _sparseCheckout = toNixExpr . map quote -> sparseCheckout,
      _furl = quote -> url,
      _name = nameField -> n
    } ->
      [trimming|
          fetchgit {
            url = $url;
            rev = $rev;
            fetchSubmodules = $fetchSubmodules;
            deepClone = $deepClone;
            leaveDotGit = $leaveDotGit;
            sparseCheckout = $sparseCheckout;$n
            sha256 = $sha256;
          }
    |]
  FetchGitHub
    { _sha256 = coerce quote -> sha256,
      _rev = quote . toNixExpr -> rev,
      _fetchSubmodules = toNixExpr -> fetchSubmodules,
      _deepClone = toNixExpr -> deepClone,
      _leaveDotGit = toNixExpr -> leaveDotGit,
      _sparseCheckout = toNixExpr . map quote -> sparseCheckout,
      _fowner = quote -> owner,
      _frepo = quote -> repo,
      _name = nameField -> n
    } ->
      -- TODO: fix fetchFromGitHub in Nixpkgs so that deepClone, leaveDotGit
      -- and sparseCheckout won't get passed to fetchzip
      if (deepClone == "true") || (leaveDotGit == "true") || (sparseCheckout /= "[ ]")
        then
          [trimming|
               fetchFromGitHub {
                 owner = $owner;
                 repo = $repo;
                 rev = $rev;
                 fetchSubmodules = $fetchSubmodules;
                 deepClone = $deepClone;
                 leaveDotGit = $leaveDotGit;
                 sparseCheckout = $sparseCheckout;$n
                 sha256 = $sha256;
               }
         |]
        else
          [trimming|
               fetchFromGitHub {
                 owner = $owner;
                 repo = $repo;
                 rev = $rev;
                 fetchSubmodules = $fetchSubmodules;$n
                 sha256 = $sha256;
               }
         |]
  (FetchUrl (quote -> url) (nameField -> n) (coerce quote -> sha256)) ->
    [trimming|
          fetchurl {
            url = $url;$n
            sha256 = $sha256;
          }
    |]
  (FetchTarball (quote -> url) (coerce quote -> sha256)) ->
    [trimming|
          fetchTarball {
            url = $url;
            sha256 = $sha256;
          }
    |]
  FetchDocker
    { _imageName = quote . toNixExpr -> imageName,
      _imageTag = quote . toNixExpr -> imageTag,
      _imageDigest = ContainerDigest (quote . toNixExpr -> imageDigest),
      _sha256 = coerce quote -> sha256,
      _fos = optionalStr "os" -> os,
      _farch = optionalStr "arch" -> arch,
      _finalImageName = optionalStr "finalImageName" -> finalImageName,
      _finalImageTag = maybe imageTag (quote . toNixExpr) -> finalImageTag,
      _tlsVerify = optionalField "tlsVerify" -> tlsVerify
    } ->
      [trimming|
            dockerTools.pullImage {
              imageName = $imageName;
              imageDigest = $imageDigest;
              sha256 = $sha256;
              finalImageTag = $finalImageTag;$os$arch$finalImageName$tlsVerify
            }
      |]
  where
    optionalField n = maybe "" (\x -> "\n" <> n <> " = " <> toNixExpr x <> ";")
    optionalStr n = optionalField n . fmap quote
    nameField = optionalStr "name"

-- | Create a trivial drv that extracts the source from a fetcher
-- TODO: Avoid using @NIX_PATH@
fetcherToDrv :: NixFetcher Fetched -> Text -> NixExpr
fetcherToDrv (toNixExpr -> fetcherExpr) (quote -> drvName) =
  [trimming|
    with import <nixpkgs> { };
    stdenv.mkDerivation {
      name = $drvName;
      src = $fetcherExpr;
      nativeBuildInputs = [ unzip ];
      dontBuild = true;
      installPhase = ''
        mkdir $$out
        cp -r * $$out
      '';
    }
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
        cargoLocks <- _prcargolock
        let depsSnippet (deps :: HashMap Text Checksum) =
              T.unlines
                [ quoteIfNeeds name
                    <> " = "
                    <> quote (coerce sum)
                    <> ";"
                  | (name, sum) <- HMap.toList deps
                ]
            lockSnippet ((T.pack -> fp) :: FilePath, (nixFP :: NixExpr, deps :: HashMap Text Checksum)) =
              let hashes = depsSnippet deps
               in [trimming|
                    cargoLock."$fp" = {
                      lockFile = $nixFP;
                      outputHashes = {
                        $hashes
                      };
                    };
                |]
        pure . T.unlines $ lockSnippet <$> HMap.toList cargoLocks
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
      date = maybe "" (\d -> "date = " <> quote d <> ";") _prgitdate
      joined = extract <> cargo <> passthru <> date
      appending = if T.null joined then "" else "\n" <> joined
