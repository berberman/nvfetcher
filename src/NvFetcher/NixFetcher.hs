{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- 'NixFetcher' is used to describe how to fetch package sources.
--
-- There are two types of fetchers overall:
--
-- 1. 'FetchGit' -- nix-prefetch-git
-- 2. 'FetchUrl' -- nix-prefetch-url
--
-- As you can see the type signature of 'prefetch':
-- a fetcher will be filled with the fetch result (hash) after the prefetch.
module NvFetcher.NixFetcher
  ( -- * Types
    NixFetcher (..),
    Prefetch (..),
    ToNixExpr (..),
    PrefetchResult,

    -- * Rules
    prefetchRule,
    prefetch,

    -- * Functions
    gitHubFetcher,
    pypiFetcher,
    gitHubReleaseFetcher,
    gitFetcher,
    urlFetcher,
  )
where

import Control.Monad (void, (<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Coerce (coerce)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import NeatInterpolation (trimming)
import NvFetcher.Types

--------------------------------------------------------------------------------

-- | Types can be converted into nix expr
class ToNixExpr a where
  toNixExpr :: a -> NixExpr

instance ToNixExpr (NixFetcher Fresh) where
  toNixExpr = buildNixFetcher "lib.fakeSha256"

instance ToNixExpr (NixFetcher Prefetched) where
  -- add quotation marks
  toNixExpr f = buildNixFetcher (T.pack $ show $ T.unpack $ coerce $ _sha256 f) f

instance ToNixExpr Bool where
  toNixExpr True = "true"
  toNixExpr False = "false"

instance ToNixExpr Version where
  toNixExpr = coerce

runFetcher :: NixFetcher Fresh -> Action SHA256
runFetcher = \case
  FetchGit {..} -> do
    let parser = A.withObject "nix-prefetch-git" $ \o -> SHA256 <$> o A..: "sha256"
    (CmdTime t, Stdout out, CmdLine c) <-
      command [EchoStderr False] "nix-prefetch-git" $
        [T.unpack _furl]
          <> ["--rev", T.unpack $ coerce _rev]
          <> ["--fetch-submodules" | _fetchSubmodules]
          <> concat [["--branch-name", T.unpack b] | b <- maybeToList _branch]
          <> ["--deepClone" | _deepClone]
          <> ["--leave-dotGit" | _leaveDotGit]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    let result = A.parseMaybe parser <=< A.decodeStrict $ out
    case result of
      Just x -> pure x
      _ -> fail $ "Failed to parse output from nix-prefetch-git: " <> T.unpack (T.decodeUtf8 out)
  FetchUrl {..} -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <- command [EchoStderr False] "nix-prefetch-url" [T.unpack _furl]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case takeWhile (not . T.null) $ reverse $ T.lines out of
      [x] -> pure $ coerce x
      _ -> fail $ "Failed to parse output from nix-prefetch-url: " <> T.unpack out

buildNixFetcher :: Text -> NixFetcher k -> Text
buildNixFetcher sha256 = \case
  FetchGit
    { _sha256 = _,
      _rev = toNixExpr -> rev,
      _fetchSubmodules = toNixExpr -> fetchSubmodules,
      _deepClone = toNixExpr -> deepClone,
      _leaveDotGit = toNixExpr -> leaveDotGit,
      ..
    } ->
      [trimming|
          fetchgit {
            url = "$_furl";
            rev = "$rev";
            fetchSubmodules = $fetchSubmodules;
            deepClone = $deepClone;
            leaveDotGit = $leaveDotGit;
            sha256 = $sha256;
          }
    |]
  (FetchUrl url _) ->
    [trimming|
          fetchurl {
            sha256 = $sha256;
            url = "$url";
          }
    |]

pypiUrl :: Text -> Version -> Text
pypiUrl pypi (coerce -> ver) =
  let h = T.cons (T.head pypi) ""
   in [trimming|https://pypi.io/packages/source/$h/$pypi/$pypi-$ver.tar.gz|]

--------------------------------------------------------------------------------

-- | Rules of nix fetcher
prefetchRule :: Rules ()
prefetchRule = void $
  addOracleCache $ \(f :: NixFetcher Fresh) -> do
    sha256 <- runFetcher f
    pure $ f {_sha256 = sha256}

-- | Run nix fetcher
prefetch :: NixFetcher Fresh -> Action (NixFetcher Prefetched)
prefetch = askOracle

--------------------------------------------------------------------------------

-- | Create a fetcher from git url
gitFetcher :: Text -> PackageFetcher
gitFetcher furl rev = FetchGit furl rev Nothing False False False ()

-- | Create a fetcher from github repo
gitHubFetcher ::
  -- | owner and repo
  (Text, Text) ->
  PackageFetcher
gitHubFetcher (owner, repo) = gitFetcher [trimming|https://github.com/$owner/$repo|]

-- | Create a fetcher from pypi
pypiFetcher :: Text -> PackageFetcher
pypiFetcher p v = urlFetcher $ pypiUrl p v

-- | Create a fetcher from github release
gitHubReleaseFetcher ::
  -- | owner and repo
  (Text, Text) ->
  -- | file name
  Text ->
  PackageFetcher
gitHubReleaseFetcher (owner, repo) fp (coerce -> ver) =
  urlFetcher
    [trimming|https://github.com/$owner/$repo/releases/download/$ver/$fp|]

-- | Create a fetcher from url
urlFetcher :: Text -> NixFetcher Fresh
urlFetcher = flip FetchUrl ()
