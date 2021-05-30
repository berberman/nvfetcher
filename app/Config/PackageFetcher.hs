{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config.PackageFetcher (fetcherCodec) where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.Default (Default, def)
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

--------------------------------------------------------------------------------

data GitOptions = GitOptions
  { goBranch :: Branch,
    goDeepClone :: Maybe Bool,
    goFetchSubmodules :: Maybe Bool,
    goLeaveDotGit :: Maybe Bool
  }
  deriving (Eq, Generic, Default)

gitOptionsCodec :: TomlCodec GitOptions
gitOptionsCodec =
  GitOptions
    <$> diwrap (dioptional (text "git.branch")) .= goBranch
    <*> dioptional (bool "git.deepClone") .= goDeepClone
    <*> dioptional (bool "git.fetchSubmodules") .= goFetchSubmodules
    <*> dioptional (bool "git.leaveDotGit") .= goLeaveDotGit

_GitOptions :: Traversal' (NixFetcher f) GitOptions
_GitOptions f x@FetchGit {..} =
  ( \GitOptions {..} ->
      x & branch .~ goBranch
        & deepClone .~ fromMaybe False goDeepClone
        & fetchSubmodules .~ fromMaybe False goFetchSubmodules
        & leaveDotGit .~ fromMaybe False goLeaveDotGit
  )
    <$> f (GitOptions _branch (Just _deepClone) (Just _fetchSubmodules) (Just _leaveDotGit))
_GitOptions _ x@FetchUrl {} = pure x

--------------------------------------------------------------------------------

gitHubICodec :: TomlCodec (Version -> NixFetcher 'Fresh)
gitHubICodec =
  textBy
    unsupportError
    ( \t -> case T.split (== '/') t of
        [owner, repo] -> Right $ gitHubFetcher (owner, repo)
        _ -> Left "unexpected github fetcher: it should be something like [owner]/[repo]"
    )
    "fetch.github"

gitHubCodec :: TomlCodec (Version -> NixFetcher Fresh)
gitHubCodec =
  dimap
    ( \f -> let fake = f "$ver" in (f, fromMaybe def $ fake ^? _GitOptions)
    )
    (\(f, g) v -> f v & _GitOptions .~ g)
    $ (,) <$> gitHubICodec .= view _1 <*> gitOptionsCodec .= view _2

--------------------------------------------------------------------------------
gitICodec :: TomlCodec (Version -> NixFetcher 'Fresh)
gitICodec =
  textBy
    unsupportError
    (Right . gitFetcher)
    "fetch.git"

gitCodec :: TomlCodec (Version -> NixFetcher Fresh)
gitCodec =
  dimap
    ( \f -> let fake = f "$ver" in (f, fromMaybe def $ fake ^? _GitOptions)
    )
    (\(f, g) v -> f v & _GitOptions .~ g)
    $ (,) <$> gitICodec .= view _1 <*> gitOptionsCodec .= view _2

--------------------------------------------------------------------------------

fetcherCodec :: TomlCodec PackageFetcher
fetcherCodec =
  gitHubCodec
    <|> Toml.textBy
      unsupportError
      (Right . pypiFetcher)
      "fetch.pypi"
    <|> gitCodec
    <|> Toml.textBy
      unsupportError
      (\t -> Right $ \(coerce -> v) -> urlFetcher $ T.replace "$ver" v t)
      "fetch.url"
