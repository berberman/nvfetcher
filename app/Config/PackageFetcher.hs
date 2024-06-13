{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config.PackageFetcher (fetcherDecoder, fetcherKeys) where

import Config.Common
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import NvFetcher.NixFetcher
import NvFetcher.Types
import NvFetcher.Types.Lens
import TOML

fetcherDecoder :: Decoder PackageFetcher
fetcherDecoder =
  asum
    [ gitHubDecoder,
      pypiDecoder,
      openVsxDecoder,
      vscodeMarketplaceDecoder,
      gitDecoder,
      urlDecoder,
      tarballDecoder,
      dockerDecoder
    ]

fetcherKeys :: [Text]
fetcherKeys =
  [ "github",
    "pypi",
    "openvsx",
    "vsmarketplace",
    "git",
    "url",
    "tarball",
    "docker"
  ]

--------------------------------------------------------------------------------

data GitOptions = GitOptions
  { goDeepClone :: Maybe Bool,
    goFetchSubmodules :: Maybe Bool,
    goLeaveDotGit :: Maybe Bool,
    goSparseCheckout :: Maybe [Text]
  }
  deriving (Eq, Generic)

gitOptionsDecoder :: Decoder GitOptions
gitOptionsDecoder =
  GitOptions
    <$> getFieldsOpt ["git", "deepClone"]
    <*> getFieldsOpt ["git", "fetchSubmodules"]
    <*> getFieldsOpt ["git", "leaveDotGit"]
    <*> getFieldsOpt ["git", "sparseCheckout"]

_GitOptions :: Traversal' (NixFetcher f) GitOptions
_GitOptions f x@FetchGit {..} =
  ( \GitOptions {..} ->
      x
        & deepClone .~ fromMaybe False goDeepClone
        & fetchSubmodules .~ fromMaybe False goFetchSubmodules
        & leaveDotGit .~ fromMaybe False goLeaveDotGit
        & sparseCheckout .~ fromMaybe [] goSparseCheckout
  )
    <$> f (GitOptions (Just _deepClone) (Just _fetchSubmodules) (Just _leaveDotGit) (Just _sparseCheckout))
_GitOptions f x@FetchGitHub {..} =
  ( \GitOptions {..} ->
      x
        & deepClone .~ fromMaybe False goDeepClone
        & fetchSubmodules .~ fromMaybe False goFetchSubmodules
        & leaveDotGit .~ fromMaybe False goLeaveDotGit
        & sparseCheckout .~ fromMaybe [] goSparseCheckout
  )
    <$> f (GitOptions (Just _deepClone) (Just _fetchSubmodules) (Just _leaveDotGit) (Just _sparseCheckout))
_GitOptions _ x = pure x

--------------------------------------------------------------------------------

gitHubDecoder :: Decoder PackageFetcher
gitHubDecoder = do
  (owner, repo) <- getFieldsWith gitHubNameDecoder ["fetch", "github"]
  gitOptions <- gitOptionsDecoder
  pure $ \v -> gitHubFetcher (owner, repo) v & _GitOptions .~ gitOptions

--------------------------------------------------------------------------------

gitDecoder :: Decoder PackageFetcher
gitDecoder = do
  url <- getFields ["fetch", "git"]
  gitOptions <- gitOptionsDecoder
  pure $ \v -> gitFetcher url v & _GitOptions .~ gitOptions

--------------------------------------------------------------------------------

pypiDecoder :: Decoder PackageFetcher
pypiDecoder = pypiFetcher <$> getFields ["fetch", "pypi"]

--------------------------------------------------------------------------------

openVsxDecoder :: Decoder PackageFetcher
openVsxDecoder = openVsxFetcher <$> getFieldsWith vscodeExtensionNameDecoder ["fetch", "openvsx"]

--------------------------------------------------------------------------------

vscodeMarketplaceDecoder :: Decoder PackageFetcher
vscodeMarketplaceDecoder = vscodeMarketplaceFetcher <$> getFieldsWith vscodeExtensionNameDecoder ["fetch", "vsmarketplace"]

--------------------------------------------------------------------------------

urlDecoder :: Decoder PackageFetcher
urlDecoder = do
  url <- getFields ["fetch", "url"]
  name <- getFieldsOpt ["url", "name"]
  pure $ \(coerce -> v) -> urlFetcher' (T.replace "$ver" v url) name

--------------------------------------------------------------------------------

tarballDecoder :: Decoder PackageFetcher
tarballDecoder = do
  url <- getFields ["fetch", "tarball"]
  pure $ \(coerce -> v) -> tarballFetcher $ T.replace "$ver" v url

--------------------------------------------------------------------------------

dockerDecoder :: Decoder PackageFetcher
dockerDecoder =
  (\f (coerce -> v) -> f & imageTag .~ v)
    <$> ( FetchDocker
            <$> getFields ["fetch", "docker"]
            <*> pure "" -- set in fmap
            <*> pure ()
            <*> pure ()
            <*> getFieldsOpt ["docker", "os"]
            <*> getFieldsOpt ["docker", "arch"]
            <*> getFieldsOpt ["docker", "finalImageName"]
            <*> getFieldsOpt ["docker", "finalImageTag"]
            <*> getFieldsOpt ["docker", "tlsVerify"]
        )
