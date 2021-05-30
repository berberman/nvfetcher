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
import Data.Void (absurd)
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

instance ToNixExpr (PostFetcher Fresh) where
  toNixExpr (RustLegacy name (coerce -> version) fetcher _) = fetchCargoTarball (name <> "-" <> version) "lib.fakeSha256" fetcher
  toNixExpr (Go bot) = absurd bot

instance ToNixExpr (PostFetcher Fetched) where
  toNixExpr (RustLegacy name (coerce -> version) fetcher sha256) = fetchCargoTarball (name <> "-" <> version) (asString $ coerce sha256) fetcher
  toNixExpr (Go bot) = absurd bot

fetchCargoTarball :: PackageName -> Text -> NixFetcher Fetched -> NixExpr
fetchCargoTarball (asString -> name) sha256 (toNixExpr -> fetcherExpr) =
  [trimming|
    rustPlatform.fetchCargoTarball {
      name = $name;
      src = $fetcherExpr;
      sha256 = $sha256;
    }
  |]
