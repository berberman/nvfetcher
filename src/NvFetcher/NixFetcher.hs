{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021-2022 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- 'NixFetcher' is used to describe how to fetch package sources.
--
-- There are five types of fetchers overall:
--
-- 1. 'FetchGit' -- nix-prefetch-git
-- 2. 'FetchGitHub' -- nix-prefetch-git/nix-prefetch-url
-- 3. 'FetchUrl' -- nix-prefetch-url
-- 4. 'FetchTarball' -- nix-prefetch-url
-- 5. 'FetchDocker' -- nix-prefetch-docker
--
-- As you can see the type signature of 'prefetch':
-- a fetcher will be filled with the fetch result (hash) after the prefetch.
module NvFetcher.NixFetcher
  ( -- * Types
    RunFetch (..),
    ForceFetch (..),
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
    gitHubReleaseFetcher',
    gitFetcher,
    urlFetcher,
    urlFetcher',
    openVsxFetcher,
    vscodeMarketplaceFetcher,
    tarballFetcher,
  )
where

import Control.Exception (ErrorCall)
import Control.Monad (void, when)
import qualified Data.Aeson as A
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import Prettyprinter (pretty, (<+>))

--------------------------------------------------------------------------------

sha256ToSri :: Text -> Action Checksum
sha256ToSri sha256 = do
  (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <-
    quietly $
      command [EchoStderr False] "nix" ["hash", "to-sri", "--type", "sha256", T.unpack sha256]
  putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
  case takeWhile (not . T.null) $ reverse $ T.lines out of
    [x] -> pure $ coerce x
    _ -> fail $ "Failed to parse output from nix hash to-sri: " <> T.unpack out

runNixPrefetchUrl :: Text -> Bool -> Maybe Text -> Action Checksum
runNixPrefetchUrl url unpack name = do
  (CmdTime t, Stdout (T.decodeUtf8 -> out), CmdLine c) <-
    quietly $
      command [EchoStderr False] "nix-prefetch-url" $
        [T.unpack url]
          <> ["--unpack" | unpack]
          <> concat [["--name", T.unpack name] | Just name <- [name]]
  putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
  case takeWhile (not . T.null) $ reverse $ T.lines out of
    [x] -> sha256ToSri x
    _ -> fail $ "Failed to parse output from nix-prefetch-url: " <> T.unpack out

newtype FetchedGit = FetchedGit {sha256 :: Text}
  deriving (Show, Generic, A.FromJSON)

runNixPrefetchGit :: Text -> Text -> Bool -> Bool -> Bool -> [Text] -> Action Checksum
runNixPrefetchGit url rev fetchSubmodules deepClone leaveDotGit sparseCheckout = do
  (CmdTime t, Stdout out, CmdLine c) <-
    quietly $
      command [EchoStderr False] "nix-prefetch-git" $
        ["--url", T.unpack url]
          <> ["--rev", T.unpack rev]
          <> ["--fetch-submodules" | fetchSubmodules]
          <> ["--deepClone" | deepClone]
          <> ["--leave-dotGit" | leaveDotGit]
          <> if null sparseCheckout then [] else ["--sparse-checkout", T.unpack $ T.intercalate "\n" sparseCheckout]
  putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
  case A.eitherDecode out of
    Right (FetchedGit x) -> sha256ToSri x
    Left e -> fail $ "Failed to parse output from nix-prefetch-git as JSON: " <> e

--------------------------------------------------------------------------------

runFetcher :: NixFetcher Fresh -> Action (NixFetcher Fetched)
runFetcher = \case
  FetchGit {..} -> do
    result <- runNixPrefetchGit _furl (coerce _rev) _fetchSubmodules _deepClone _leaveDotGit _sparseCheckout
    pure FetchGit {_sha256 = coerce result, ..}
  FetchGitHub {..} -> do
    let useFetchGit = _fetchSubmodules || _leaveDotGit || _deepClone || not (null _sparseCheckout)
        ver = coerce _rev
    result <-
      if useFetchGit
        then runNixPrefetchGit [trimming|https://github.com/$_fowner/$_frepo|] (coerce _rev) _fetchSubmodules _deepClone _leaveDotGit _sparseCheckout
        else runNixPrefetchUrl [trimming|https://github.com/$_fowner/$_frepo/archive/$ver.tar.gz|] True mempty
    pure FetchGitHub {_sha256 = result, ..}
  FetchUrl {..} -> do
    result <- runNixPrefetchUrl _furl False _name
    pure FetchUrl {_sha256 = result, ..}
  FetchTarball {..} -> do
    result <- runNixPrefetchUrl _furl True mempty
    pure FetchTarball {_sha256 = result, ..}
  FetchDocker {..} -> do
    (CmdTime t, Stdout out, CmdLine c) <-
      quietly $
        command [EchoStderr False] "nix-prefetch-docker" $
          [ "--json",
            T.unpack _imageName,
            T.unpack _imageTag
          ]
            <> concat [["--os", T.unpack os] | Just os <- [_fos]]
            <> concat [["--arch", T.unpack arch] | Just arch <- [_farch]]
    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
    case A.eitherDecode out of
      Right FetchedContainer {..} -> do
        sri <- sha256ToSri sha256
        pure FetchDocker {_sha256 = sri, _imageDigest = imageDigest, ..}
      Left e -> fail $ "Failed to parse output from nix-prefetch-docker as JSON: " <> e

data FetchedContainer = FetchedContainer
  { imageDigest :: ContainerDigest,
    sha256 :: Text
  }
  deriving (Show, Generic, A.FromJSON)

pypiUrl :: Text -> Version -> Text
pypiUrl pypi (coerce -> ver) =
  let h = T.cons (T.head pypi) ""
   in [trimming|https://pypi.org/packages/source/$h/$pypi/$pypi-$ver.tar.gz|]

--------------------------------------------------------------------------------

-- | Rules of nix fetcher
prefetchRule :: Rules ()
prefetchRule = void $
  addOracleCache $ \(RunFetch force f) -> do
    when (force == ForceFetch) alwaysRerun
    putInfo . show $ "#" <+> pretty f
    keepGoing <- nvcheckerKeepGoing
    if keepGoing
      then -- If fetch failed, always rerun and return Nothing
      actionCatch (fmap Just <$> withRetry $ runFetcher f) $ \(e :: ErrorCall) -> do
        alwaysRerun
        putError $ show e <> "\nKeep going..."
        pure Nothing
      else fmap Just <$> withRetry $ runFetcher f

-- | Run nix fetcher
prefetch :: NixFetcher Fresh -> ForceFetch -> Action (Maybe (NixFetcher Fetched))
prefetch f force = askOracle $ RunFetch force f

--------------------------------------------------------------------------------

-- | Create a fetcher from git url
gitFetcher :: Text -> PackageFetcher
gitFetcher furl rev = FetchGit furl rev False True False [] Nothing ()

-- | Create a fetcher from github repo
gitHubFetcher ::
  -- | owner and repo
  (Text, Text) ->
  PackageFetcher
gitHubFetcher (owner, repo) rev = FetchGitHub owner repo rev False False False [] Nothing ()

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
gitHubReleaseFetcher (owner, repo) fp = gitHubReleaseFetcher' (owner, repo) $ const fp

-- | Create a fetcher from github release
gitHubReleaseFetcher' ::
  -- | owner and repo
  (Text, Text) ->
  -- | file name computed from version
  (Version -> Text) ->
  PackageFetcher
gitHubReleaseFetcher' (owner, repo) f (coerce -> ver) =
  let fp = f $ coerce ver
   in urlFetcher
        [trimming|https://github.com/$owner/$repo/releases/download/$ver/$fp|]

-- | Create a fetcher from url
urlFetcher :: Text -> NixFetcher Fresh
urlFetcher url = FetchUrl url Nothing ()

-- | Create a fetcher from url specifying the file name
urlFetcher' :: Text -> Maybe Text -> NixFetcher Fresh
urlFetcher' url name = FetchUrl url name ()

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
