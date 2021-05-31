{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module NvFetcher.NixExpr
  ( NixExpr,
    ToNixExpr (..),
  )
where

import Data.Coerce (coerce)
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
            sha256 = $sha256;
            url = "$url";
          }
    |]

instance ToNixExpr ExtractSrc where
  toNixExpr (ExtractSrc fetcher files) = extractFiles fetcher files

extractFiles :: NixFetcher Fetched -> [FilePath] -> NixExpr
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
