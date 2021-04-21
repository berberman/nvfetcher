{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import qualified Data.Text as T
import Development.NvFetcher
import Toml (TomlCodec, (.=))
import qualified Toml

newtype NvFetcherConfig = NvFetcherConfig {getPackages :: [Package]}
  deriving (Show)

nvfetcherConfigCodec :: TomlCodec NvFetcherConfig
nvfetcherConfigCodec = NvFetcherConfig <$> Toml.list packageCodec "package" .= getPackages

packageCodec :: TomlCodec Package
packageCodec =
  Package
    <$> Toml.diwrap (Toml.text "name") .= pname
      <*> versionSourceCodec .= pversion
      <*> fetcherCodec .= pfetcher

versionSourceCodec :: TomlCodec VersionSource
versionSourceCodec =
  Toml.dimatch
    ( \case
        GitHub {..} -> Just $ owner <> "/" <> repo
        _ -> Nothing
    )
    ( \x -> case T.split (== '/') x of
        [owner, repo] -> GitHub {..}
        _ -> error "parse error on src.github"
    )
    (Toml.text "src.github")
    <|> Toml.dimatch
      ( \case
          Pypi {..} -> Just pypi
          _ -> Nothing
      )
      Pypi
      (Toml.text "src.pypi")
    <|> Toml.dimatch
      ( \case
          ArchLinux {..} -> Just archpkg
          _ -> Nothing
      )
      ArchLinux
      (Toml.text "src.archpkg")
    <|> Toml.dimatch
      ( \case
          Aur {..} -> Just aur
          _ -> Nothing
      )
      Aur
      (Toml.text "src.aur")
    <|> Toml.dimatch
      ( \case
          Manual {..} -> Just manual
          _ -> Nothing
      )
      Manual
      (Toml.text "src.manual")

-- | Use it only for deserialization!!!
fetcherCodec :: TomlCodec (Version -> NixFetcher Fresh)
fetcherCodec =
  Toml.dimatch
    ( const Nothing -- serialization is unsupported
    )
    ( \t v -> case T.split (== '/') t of
        [owner, rest] -> case T.split (== ':') rest of
          [repo, coerce . T.replace "$ver" (coerce v) -> v'] ->
            gitHubFetcher (owner, repo) v'
          [repo] -> gitHubFetcher (owner, repo) v
          _ -> error "parse error on fetch.github (A)"
        _ -> error "parse error on fetch.github (B)"
    )
    (Toml.text "fetch.github")
    <|> Toml.dimatch
      ( const Nothing -- serialization is unsupported
      )
      ( \t v -> case T.split (== ':') t of
          [fpypi, coerce . T.replace "$ver" (coerce v) -> v'] ->
            pypiFetcher fpypi v'
          [fpypi] -> pypiFetcher fpypi v
          _ -> error "parse error on fetch.pypi"
      )
      (Toml.text "fetch.pypi")
    <|> Toml.dimatch
      ( \f -> case f "$ver" of
          FetchGit {..} -> Just $ furl <> ":" <> coerce rev
          _ -> Nothing
      )
      ( \t v -> case T.split (== ':') t of
          [furl, coerce . T.replace "$ver" (coerce v) -> v'] ->
            gitFetcher furl v'
          [furl] -> gitFetcher furl $ coerce v
          _ -> error "parse error on fetch.git"
      )
      (Toml.text "fetch.git")
    <|> Toml.dimatch
      ( \f -> case f "$ver" of
          FetchUrl {..} -> Just furl
          _ -> Nothing
      )
      ( \t v -> FetchUrl (T.replace "$ver" (coerce v) t) ()
      )
      (Toml.text "fetch.url")
