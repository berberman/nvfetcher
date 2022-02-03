{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config.PackageFetcher
  ( fetcherCodec,
    fetcherKeys,
  )
where

import Data.Coerce (coerce)
import Data.Default (Default, def)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NvFetcher.NixFetcher
import NvFetcher.Types
import NvFetcher.Types.Lens
import Toml

unsupportError :: a
unsupportError = error "serialization is unsupported"

fetcherCodec :: TomlCodec PackageFetcher
fetcherCodec =
  asum
    [ gitHubCodec,
      pypiCodec,
      openVsxCodec,
      vscodeMarketplaceCodec,
      gitCodec,
      urlCodec,
      tarballCodec
    ]

fetcherKeys :: [Key]
fetcherKeys =
  [ "fetch.github",
    "fetch.pypi",
    "fetch.openvsx",
    "fetch.vsmarketplace",
    "fetch.git",
    "fetch.url",
    "fetch.tarball"
  ]

--------------------------------------------------------------------------------

data GitOptions = GitOptions
  { goDeepClone :: Maybe Bool,
    goFetchSubmodules :: Maybe Bool,
    goLeaveDotGit :: Maybe Bool
  }
  deriving (Eq, Generic, Default)

gitOptionsCodec :: TomlCodec GitOptions
gitOptionsCodec =
  GitOptions
    <$> dioptional (bool "git.deepClone") .= goDeepClone
    <*> dioptional (bool "git.fetchSubmodules") .= goFetchSubmodules
    <*> dioptional (bool "git.leaveDotGit") .= goLeaveDotGit

_GitOptions :: Traversal' (NixFetcher f) GitOptions
_GitOptions f x@FetchGit {..} =
  ( \GitOptions {..} ->
      x
        & deepClone .~ fromMaybe False goDeepClone
        & fetchSubmodules .~ fromMaybe False goFetchSubmodules
        & leaveDotGit .~ fromMaybe False goLeaveDotGit
  )
    <$> f (GitOptions (Just _deepClone) (Just _fetchSubmodules) (Just _leaveDotGit))
_GitOptions f x@FetchGitHub {..} =
  ( \GitOptions {..} ->
      x
        & deepClone .~ fromMaybe False goDeepClone
        & fetchSubmodules .~ fromMaybe False goFetchSubmodules
        & leaveDotGit .~ fromMaybe False goLeaveDotGit
  )
    <$> f (GitOptions (Just _deepClone) (Just _fetchSubmodules) (Just _leaveDotGit))
_GitOptions _ x = pure x

--------------------------------------------------------------------------------

gitHubICodec :: TomlCodec PackageFetcher
gitHubICodec =
  textBy
    unsupportError
    ( \t -> case T.split (== '/') t of
        [owner, repo] -> Right $ gitHubFetcher (owner, repo)
        _ -> Left "unexpected github fetcher: it should be something like [owner]/[repo]"
    )
    "fetch.github"

gitHubCodec :: TomlCodec PackageFetcher
gitHubCodec =
  dimap
    ( \f -> let fake = f "$ver" in (f, fromMaybe def $ fake ^? _GitOptions)
    )
    (\(f, g) v -> f v & _GitOptions .~ g)
    $ (,) <$> gitHubICodec .= view _1 <*> gitOptionsCodec .= view _2

--------------------------------------------------------------------------------
gitICodec :: TomlCodec PackageFetcher
gitICodec =
  textBy
    unsupportError
    (Right . gitFetcher)
    "fetch.git"

gitCodec :: TomlCodec PackageFetcher
gitCodec =
  dimap
    ( \f -> let fake = f "$ver" in (f, fromMaybe def $ fake ^? _GitOptions)
    )
    (\(f, g) v -> f v & _GitOptions .~ g)
    $ (,) <$> gitICodec .= view _1 <*> gitOptionsCodec .= view _2

--------------------------------------------------------------------------------
pypiCodec :: TomlCodec PackageFetcher
pypiCodec =
  Toml.textBy
    unsupportError
    (Right . pypiFetcher)
    "fetch.pypi"

--------------------------------------------------------------------------------

openVsxCodec :: TomlCodec PackageFetcher
openVsxCodec =
  textBy
    unsupportError
    ( \t -> case T.split (== '.') t of
        -- assume we can't have '.' in extension's name
        [publisher, extName] -> Right $ openVsxFetcher (publisher, extName)
        _ -> Left "unexpected openvsx fetcher: it should be something like [publisher]/[extName]"
    )
    "fetch.openvsx"

--------------------------------------------------------------------------------

vscodeMarketplaceCodec :: TomlCodec PackageFetcher
vscodeMarketplaceCodec =
  textBy
    unsupportError
    ( \t -> case T.split (== '.') t of
        -- assume we can't have '.' in extension's name
        [publisher, extName] -> Right $ vscodeMarketplaceFetcher (publisher, extName)
        _ -> Left "unexpected vscode marketplace fetcher: it should be something like [publisher]/[extName]"
    )
    "fetch.vsmarketplace"

--------------------------------------------------------------------------------
urlCodec :: TomlCodec PackageFetcher
urlCodec =
  Toml.textBy
    unsupportError
    (\t -> Right $ \(coerce -> v) -> urlFetcher $ T.replace "$ver" v t)
    "fetch.url"

--------------------------------------------------------------------------------

tarballCodec :: TomlCodec PackageFetcher
tarballCodec =
  Toml.textBy
    unsupportError
    (\t -> Right $ \(coerce -> v) -> tarballFetcher $ T.replace "$ver" v t)
    "fetch.tarball"
