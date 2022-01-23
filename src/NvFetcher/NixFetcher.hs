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
-- There are three types of fetchers overall:
--
-- 1. 'FetchGit' -- nix-prefetch fetchgit
-- 2. 'FetchGitHub' -- nix-prefetch fetchFromGitHub
-- 3. 'FetchUrl' -- nix-prefetch fetchurl
--
-- As you can see the type signature of 'prefetch':
-- a fetcher will be filled with the fetch result (hash) after the prefetch.
module NvFetcher.NixFetcher
  ( -- * Types
    NixFetcher (..),
    FetchStatus (..),
    FetchResult,

    -- * Rules
    prefetchRule,
    prefetch,

    -- * Functions
    gitHubFetcher,
    pypiFetcher,
    gitHubReleaseFetcher,
    gitFetcher,
    urlFetcher,
    openVsxFetcher,
    vscodeMarketplaceFetcher,
    tarballFetcher,
  )
where

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import NeatInterpolation (trimming)
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras

--------------------------------------------------------------------------------

runFetcher :: NixFetcher Fresh -> Action Checksum
runFetcher = \case
  FetchGit {..} -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <-
      command [EchoStderr False] "nix-prefetch" $
        ["fetchgit"]
          <> ["--url", T.unpack _furl]
          <> ["--rev", T.unpack $ coerce _rev]
          <> ["--fetchSubmodules" | _fetchSubmodules]
          <> ["--deepClone" | _deepClone]
          <> ["--leaveDotGit" | _leaveDotGit]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case takeWhile (not . T.null) $ reverse $ T.lines out of
      [x] -> pure $ coerce x
      _ -> fail $ "Failed to parse output from nix-prefetch: " <> T.unpack out
  FetchGitHub {..} -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <-
      command [EchoStderr False] "nix-prefetch" $
        ["fetchFromGitHub"]
          <> ["--owner", T.unpack _fowner]
          <> ["--repo", T.unpack _frepo]
          <> ["--rev", T.unpack $ coerce _rev]
          <> ["--fetchSubmodules" | _fetchSubmodules]
          <> ["--deepClone" | _deepClone]
          <> ["--leaveDotGit" | _leaveDotGit]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case takeWhile (not . T.null) $ reverse $ T.lines out of
      [x] -> pure $ coerce x
      _ -> fail $ "Failed to parse output from nix-prefetch: " <> T.unpack out
  FetchUrl {..} -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <-
      command [EchoStderr False] "nix-prefetch" ["fetchurl", "--url", T.unpack _furl]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case takeWhile (not . T.null) $ reverse $ T.lines out of
      [x] -> pure $ coerce x
      _ -> fail $ "Failed to parse output from nix-prefetch: " <> T.unpack out
  FetchTarball {..} -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <-
      command [EchoStderr False] "nix-prefetch" ["fetchTarball", "--url", T.unpack _furl]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case takeWhile (not . T.null) $ reverse $ T.lines out of
      [x] -> pure $ coerce x
      _ -> fail $ "Failed to parse output from nix-prefetch: " <> T.unpack out

pypiUrl :: Text -> Version -> Text
pypiUrl pypi (coerce -> ver) =
  let h = T.cons (T.head pypi) ""
   in [trimming|https://pypi.io/packages/source/$h/$pypi/$pypi-$ver.tar.gz|]

--------------------------------------------------------------------------------

-- | Rules of nix fetcher
prefetchRule :: Rules ()
prefetchRule = void $
  addOracleCache $ \(f :: NixFetcher Fresh) -> do
    sha256 <- withRetries $ runFetcher f
    pure $ f {_sha256 = sha256}

-- | Run nix fetcher
prefetch :: NixFetcher Fresh -> Action (NixFetcher Fetched)
prefetch = askOracle

--------------------------------------------------------------------------------

-- | Create a fetcher from git url
gitFetcher :: Text -> PackageFetcher
gitFetcher furl rev = FetchGit furl rev False False False Nothing ()

-- | Create a fetcher from github repo
gitHubFetcher ::
  -- | owner and repo
  (Text, Text) ->
  PackageFetcher
gitHubFetcher (owner, repo) rev = FetchGitHub owner repo rev False False False Nothing ()

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
urlFetcher url = FetchUrl url Nothing ()

-- | Create a fetcher from openvsx
openVsxFetcher ::
  -- | publisher and extension name
  (Text, Text) ->
  PackageFetcher
openVsxFetcher (publisher, extName) (coerce -> ver) =
  FetchUrl
    [trimming|https://open-vsx.org/api/$publisher/$extName/$ver/file/$publisher.$extName-$ver.vsix|]
    (Just [trimming|$extName-$ver.zip|])
    ()

-- | Create a fetcher from vscode marketplace
vscodeMarketplaceFetcher ::
  -- | publisher and extension name
  (Text, Text) ->
  PackageFetcher
vscodeMarketplaceFetcher (publisher, extName) (coerce -> ver) =
  FetchUrl
    [trimming|https://$publisher.gallery.vsassets.io/_apis/public/gallery/publisher/$publisher/extension/$extName/$ver/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage|]
    (Just [trimming|$extName-$ver.zip|])
    ()

-- | Create a fetcher from url, using fetchTarball
tarballFetcher :: Text -> NixFetcher Fresh
tarballFetcher url = FetchTarball url ()
