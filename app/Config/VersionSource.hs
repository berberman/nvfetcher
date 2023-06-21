{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Config.VersionSource (versionSourceDecoder, versionSourceKeys, listOptionsKeys) where

import Config.Common
import Data.Coerce (coerce)
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as T
import NvFetcher.Types
import TOML

versionSourceDecoder :: Decoder VersionSource
versionSourceDecoder =
  -- everything is in src table
  flip getFieldWith "src" $
    asum
      [ gitHubReleaseDecoder,
        gitHubTagDecoder,
        gitDecoder,
        pypiDecoder,
        archLinuxDecoder,
        aurDecoder,
        manualDecoder,
        repologyDecoder,
        webpageDecoder,
        httpHeaderDecoder,
        openVsxDecoder,
        vscodeMarketplaceDecoder,
        cmdDecoder,
        containerDecoder
      ]

versionSourceKeys :: [Text]
versionSourceKeys =
  [ "github",
    "github_tag",
    "git",
    "pypi",
    "archpkg",
    "aur",
    "manual",
    "webpage",
    "httpheader",
    "openvsx",
    "vsmarketplace",
    "cmd",
    "container"
  ]

listOptionsKeys :: [Text]
listOptionsKeys =
  [ "include_regex",
    "exclude_regex",
    "sort_version_key",
    "ignored"
  ]

--------------------------------------------------------------------------------

listOptionsDecoder :: Decoder ListOptions
listOptionsDecoder =
  ListOptions
    <$> getFieldOpt "include_regex"
    <*> getFieldOpt "exclude_regex"
    <*> ( getFieldOpt @Text "sort_version_key" >>= \case
            Just "parse_version" -> pure $ Just ParseVersion
            Just "vercmp" -> pure $ Just Vercmp
            Just _ -> makeDecoder $ invalidValue "unexpected sort_version_key: it should be either parse_version or vercmp"
            Nothing -> pure Nothing
        )
    <*> getFieldOpt "ignored"

--------------------------------------------------------------------------------

gitHubReleaseDecoder :: Decoder VersionSource
gitHubReleaseDecoder = uncurry GitHubRelease <$> getFieldWith githubDecoder "github"

--------------------------------------------------------------------------------

gitHubTagDecoder :: Decoder VersionSource
gitHubTagDecoder = do
  (_owner, _repo) <- getFieldWith githubDecoder "github_tag"
  _listOptions <- listOptionsDecoder
  pure GitHubTag {..}

--------------------------------------------------------------------------------

gitDecoder :: Decoder VersionSource
gitDecoder = Git <$> getField "git" <*> (coerce @(Maybe Text) <$> getFieldOpt "branch")

--------------------------------------------------------------------------------

pypiDecoder :: Decoder VersionSource
pypiDecoder = Pypi <$> getField "pypi"

--------------------------------------------------------------------------------

archLinuxDecoder :: Decoder VersionSource
archLinuxDecoder = ArchLinux <$> getField "archpkg"

--------------------------------------------------------------------------------

aurDecoder :: Decoder VersionSource
aurDecoder = Aur <$> getField "aur"

--------------------------------------------------------------------------------

manualDecoder :: Decoder VersionSource
manualDecoder = Manual <$> getField "manual"

--------------------------------------------------------------------------------

repologyDecoder :: Decoder VersionSource
repologyDecoder = makeDecoder $ \case
  v@(String s) -> case T.split (== ':') s of
    [_repology, _repo] -> pure Repology {..}
    _ -> invalidValue "unexpected repology format: it should be in the format of [repology]:[repo]" v
  v -> typeMismatch v

------------------------------------------------------------

webpageDecoder :: Decoder VersionSource
webpageDecoder = do
  _vurl <- getField "webpage"
  _regex <- getField "regex"
  _listOptions <- listOptionsDecoder
  pure Webpage {..}

--------------------------------------------------------------------------------

httpHeaderDecoder :: Decoder VersionSource
httpHeaderDecoder = do
  _vurl <- getField "httpheader"
  _regex <- getField "regex"
  _listOptions <- listOptionsDecoder
  pure HttpHeader {..}

--------------------------------------------------------------------------------

openVsxDecoder :: Decoder VersionSource
openVsxDecoder = uncurry OpenVsx <$> getFieldWith vscodeExtensionDecoder "openvsx"

--------------------------------------------------------------------------------

vscodeMarketplaceDecoder :: Decoder VersionSource
vscodeMarketplaceDecoder = uncurry VscodeMarketplace <$> getFieldWith vscodeExtensionDecoder "vsmarketplace"

--------------------------------------------------------------------------------

cmdDecoder :: Decoder VersionSource
cmdDecoder = Cmd <$> getField "cmd"

--------------------------------------------------------------------------------

containerDecoder :: Decoder VersionSource
containerDecoder = Container <$> getField "container" <*> listOptionsDecoder
