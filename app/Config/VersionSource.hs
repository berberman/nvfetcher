{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.VersionSource
  ( versionSourceCodec,
    versionSourceKeys,
  )
where

import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Extra (uncurry3)
import Lens.Micro
import Lens.Micro.Extras (view)
import NvFetcher.Types
import NvFetcher.Types.Lens
import Toml

versionSourceCodec :: TomlCodec VersionSource
versionSourceCodec =
  asum
    [ gitHubReleaseCodec,
      gitHubTagCodec,
      gitCodec,
      pypiCodec,
      archLinuxCodec,
      aurCodec,
      manualCodec,
      repologyCodec,
      webpageCodec,
      httpHeaderCodec,
      openVsxCodec,
      vscodeMarketplaceCodec,
      cmdCodec
    ]

versionSourceKeys :: [Key]
versionSourceKeys =
  [ "src.github",
    "src.github_tag",
    "src.git",
    "src.pypi",
    "src.archpkg",
    "src.aur",
    "src.manual",
    "src.webpage",
    "src.httpheader",
    "src.openvsx",
    "src.vsmarketplace",
    "src.cmd"
  ]

--------------------------------------------------------------------------------

githubICodec :: Key -> TomlCodec (Text, Text)
githubICodec =
  textBy
    (\(owner, repo) -> owner <> "/" <> repo)
    ( \t ->
        case T.split (== '/') t of
          [owner, repo] -> Right (owner, repo)
          _ -> Left "unexpected github source format: it should be something like [owner]/[repo]"
    )

listOptionsCodec :: TomlCodec ListOptions
listOptionsCodec =
  ListOptions
    <$> dioptional (text "src.include_regex") .= _includeRegex
    <*> dioptional (text "src.exclude_regex") .= _excludeRegex
    <*> dioptional
      ( textBy
          (T.pack . show)
          ( \case
              "parse_version" -> Right ParseVersion
              "vercmp" -> Right Vercmp
              _ -> Left "unexpected sort_version_key: it should be either parse_version or vercmp"
          )
          "src.sort_version_key"
      )
      .= _sortVersionKey
    <*> dioptional (text "src.ignored") .= _ignored

--------------------------------------------------------------------------------

matchGitHubRelease :: VersionSource -> Maybe (Text, Text)
matchGitHubRelease x = (,) <$> x ^? owner <*> x ^? repo

gitHubReleaseCodec :: TomlCodec VersionSource
gitHubReleaseCodec = dimatch matchGitHubRelease (uncurry GitHubRelease) $ githubICodec "src.github"

--------------------------------------------------------------------------------

matchGitHubTag :: VersionSource -> Maybe ((Text, Text), ListOptions)
matchGitHubTag x = do
  o <- x ^? owner
  r <- x ^? repo
  l <- x ^? listOptions
  pure ((o, r), l)

gitHubTagICodec :: TomlCodec ((Text, Text), ListOptions)
gitHubTagICodec =
  (,) <$> githubICodec "src.github_tag" .= view _1
    <*> listOptionsCodec .= view _2

gitHubTagCodec :: TomlCodec VersionSource
gitHubTagCodec = dimatch matchGitHubTag (\((_owner, _repo), _listOptions) -> GitHubTag {..}) gitHubTagICodec

--------------------------------------------------------------------------------

matchGit :: VersionSource -> Maybe (Text, Branch)
matchGit x = (,) <$> x ^? vurl <*> x ^? vbranch

gitICodec :: TomlCodec (Text, Branch)
gitICodec = (,) <$> text "src.git" .= view _1 <*> diwrap (dioptional (text "src.branch")) .= view _2

gitCodec :: TomlCodec VersionSource
gitCodec = dimatch matchGit (uncurry Git) gitICodec

--------------------------------------------------------------------------------

matchPypi :: VersionSource -> Maybe Text
matchPypi x = x ^? pypi

pypiCodec :: TomlCodec VersionSource
pypiCodec = dimatch matchPypi Pypi (text "src.pypi")

--------------------------------------------------------------------------------

matchArchLinux :: VersionSource -> Maybe Text
matchArchLinux x = x ^? archpkg

archLinuxCodec :: TomlCodec VersionSource
archLinuxCodec = dimatch matchArchLinux ArchLinux (text "src.archpkg")

--------------------------------------------------------------------------------

matchAur :: VersionSource -> Maybe Text
matchAur x = x ^? aur

aurCodec :: TomlCodec VersionSource
aurCodec = dimatch matchAur Aur (text "src.aur")

--------------------------------------------------------------------------------

matchManual :: VersionSource -> Maybe Text
matchManual x = x ^? manual

manualCodec :: TomlCodec VersionSource
manualCodec = dimatch matchManual Manual (text "src.manual")

--------------------------------------------------------------------------------

matchRepology :: VersionSource -> Maybe (Text, Text)
matchRepology x = (,) <$> x ^? repology <*> x ^? repo

repologyICodec :: TomlCodec (Text, Text)
repologyICodec =
  textBy
    (\(repology, repo) -> repology <> ":" <> repo)
    ( \t ->
        case T.split (== ':') t of
          [owner, repo] -> Right (owner, repo)
          _ -> Left "unexpected repology source format: it should be something like [repology]:[repo]"
    )
    "src.repology"

repologyCodec :: TomlCodec VersionSource
repologyCodec = dimatch matchRepology (uncurry Repology) repologyICodec

------------------------------------------------------------

matchWebpage :: VersionSource -> Maybe (Text, Text, ListOptions)
matchWebpage x = (,,) <$> x ^? vurl <*> x ^? regex <*> x ^? listOptions

webpageICodec :: TomlCodec (Text, Text, ListOptions)
webpageICodec =
  (,,) <$> text "src.webpage" .= view _1
    <*> text "src.regex" .= view _2
    <*> listOptionsCodec .= view _3

webpageCodec :: TomlCodec VersionSource
webpageCodec = dimatch matchWebpage (uncurry3 Webpage) webpageICodec

--------------------------------------------------------------------------------

matchHttpHeader :: VersionSource -> Maybe (Text, Text, ListOptions)
matchHttpHeader x = (,,) <$> x ^? vurl <*> x ^? regex <*> x ^? listOptions

httpHeaderICodec :: TomlCodec (Text, Text, ListOptions)
httpHeaderICodec =
  (,,) <$> text "src.httpheader" .= view _1
    <*> text "src.regex" .= view _2
    <*> listOptionsCodec .= view _3

httpHeaderCodec :: TomlCodec VersionSource
httpHeaderCodec = dimatch matchHttpHeader (uncurry3 HttpHeader) httpHeaderICodec

--------------------------------------------------------------------------------

matchOpenVsx :: VersionSource -> Maybe (Text, Text)
matchOpenVsx x = (,) <$> x ^? ovPublisher <*> x ^? ovExtName

openVsxICodec :: TomlCodec (Text, Text)
openVsxICodec =
  textBy
    (\(publisher, extName) -> publisher <> "." <> extName)
    ( \t ->
        case T.split (== '.') t of
          -- assume we can't have '.' in extension's name
          [publisher, extName] -> Right (publisher, extName)
          _ -> Left "unexpected openvsx source format: it should be something like [publisher].[extName]"
    )
    "src.openvsx"

openVsxCodec :: TomlCodec VersionSource
openVsxCodec = dimatch matchOpenVsx (uncurry OpenVsx) openVsxICodec

--------------------------------------------------------------------------------

matchVscodeMarketplace :: VersionSource -> Maybe (Text, Text)
matchVscodeMarketplace x = (,) <$> x ^? vsmPublisher <*> x ^? vsmExtName

vscodeMarketplaceICodec :: TomlCodec (Text, Text)
vscodeMarketplaceICodec =
  textBy
    (\(publisher, extName) -> publisher <> "." <> extName)
    ( \t ->
        case T.split (== '.') t of
          -- assume we can't have '.' in extension's name
          [publisher, extName] -> Right (publisher, extName)
          _ -> Left "unexpected vscode marketplace source format: it should be something like [publisher].[extName]"
    )
    "src.vsmarketplace"

vscodeMarketplaceCodec :: TomlCodec VersionSource
vscodeMarketplaceCodec = dimatch matchVscodeMarketplace (uncurry VscodeMarketplace) vscodeMarketplaceICodec

--------------------------------------------------------------------------------

matchCmd :: VersionSource -> Maybe Text
matchCmd x = x ^? vcmd

cmdCodec :: TomlCodec VersionSource
cmdCodec = dimatch matchCmd Cmd (text "src.cmd")
