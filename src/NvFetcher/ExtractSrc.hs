{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module NvFetcher.ExtractSrc
  ( ExtractSrc (..),
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

extractSrcRule :: Rules ()
extractSrcRule = void $
  addOracleCache $ \(WithPackageKey (q :: ExtractSrc, _pkg)) -> withTempFile $ \fp -> do
    writeFile' fp $ T.unpack $ wrap $ toNixExpr q
    need [fp]
    -- TODO: Avoid using NIX_PATH
    (CmdTime t, StdoutTrim out, CmdLine c) <- cmd Shell $ "nix-instantiate --eval --strict --json --read-write-mode -E 'let pkgs = import <nixpkgs> { }; in ((import " <> fp <> ") pkgs)'"
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case A.decodeStrict out of
      Just x -> pure x
      _ -> fail $ "Failed to parse output of nix-instantiate: " <> T.unpack (T.decodeUtf8 out)

extractSrc :: ExtractSrc -> PackageKey -> Action (HashMap FilePath Text)
extractSrc v k = askOracle $ WithPackageKey (v, k)

--------------------------------------------------------------------------------

wrap :: NixExpr -> NixExpr
wrap expr =
  [trimming|
    { pkgs, ... }:
    $expr
  |]
