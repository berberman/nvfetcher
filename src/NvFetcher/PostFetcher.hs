{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module NvFetcher.PostFetcher
  ( -- * Rules
    postFetchRule,
    postfetch,

    -- * Functions
    calculateCargoSha256,
  )
where

import Control.Monad (void, (<=<))
import Data.Coerce (coerce)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (absurd)
import Development.Shake
import NeatInterpolation (trimming)
import NvFetcher.NixExpr
import NvFetcher.NixFetcher
import NvFetcher.Types

postFetchRule :: Rules ()
postFetchRule = void $
  addOracleCache $ \(WithPackageKey (q :: PostFetcher Fresh, _pkg)) -> case q of
    RustLegacy name version fetcher () -> withTempFile $ \fp -> do
      writeFile' fp $ T.unpack $ wrap $ toNixExpr q
      writeFile' "t.nix" $ T.unpack $ toNixExpr q
      need [fp]
      -- TODO: Avoid using NIX_PATH
      (CmdTime t0, StdoutTrim (out0 :: String), CmdLine c0) <- cmd Shell $ "nix-instantiate -E 'let pkgs = import <nixpkgs> { }; in ((import " <> fp <> ") pkgs)'"
      putVerbose $ "Finishing running " <> c0 <> ", took " <> show t0 <> "s"
      -- Should faile due to hash mismatch of FOD
      (Exit _, CmdTime t, Stderr (T.decodeUtf8 -> out), CmdLine c) <- cmd $ "nix-store -r " <> out0
      putInfo $ T.unpack out
      putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
      let result = listToMaybe [s | ((T.stripPrefix "sha256-" . T.stripStart) <=< T.stripPrefix "got:" . T.stripStart -> Just s) <- T.lines out]
      case result of
        Just x -> pure $ RustLegacy name version fetcher $ coerce x
        _ -> fail "Failed to parse output from building drv"
    Go bot -> absurd bot

postfetch :: PostFetcher Fresh -> PackageKey -> Action (PostFetcher Fetched)
postfetch v k = askOracle $ WithPackageKey (v, k)

--------------------------------------------------------------------------------

-- | Post-fetch to get @cargoSha256@
calculateCargoSha256 :: PackagePostFetch
calculateCargoSha256 name version fetcher = RustLegacy name version fetcher ()

wrap :: NixExpr -> NixExpr
wrap expr =
  [trimming|
    { pkgs, ... }:
    with pkgs;
    $expr
  |]
