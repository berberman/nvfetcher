{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Development.NvFetcher
import Toml (TOML, TomlCodec, (.=))
import qualified Toml
import Validation (validationToEither)

parseConfig :: TOML -> Either [Toml.TomlDecodeError] [Package]
parseConfig toml = go tables [] []
  where
    go (Left errs : xs) se sp = go xs (se <> errs) sp
    go (Right x : xs) se sp = go xs se (x : sp)
    go [] [] sp = Right sp
    go [] se _ = Left se
    tables = [fmap (uncurry (Package k)) $ validationToEither $ Toml.runTomlCodec iCodec v | (Toml.unKey -> (Toml.unPiece -> k) :| _, v) <- Toml.toList $ Toml.tomlTables toml]
    iCodec = (,) <$> versionSourceCodec .= fst <*> fetcherCodec .= snd

versionSourceCodec :: TomlCodec VersionSource
versionSourceCodec =
  Toml.dimatch
    ( \case
        GitHubRelease {..} -> Just GitHubRelease {..}
        _ -> Nothing
    )
    id
    ( Toml.textBy
        ( \case
            GitHubRelease {..} -> owner <> "/" <> repo
            _ -> error "impossible"
        )
        ( \x -> case T.split (== '/') x of
            [owner, repo] -> Right GitHubRelease {..}
            _ -> Left "unexpected github srouce: it should be something like [owner]/[repo]"
        )
        "src.github"
    )
    <|> Toml.dimatch
      ( \case
          Git {..} -> Just vurl
          _ -> Nothing
      )
      Git
      (Toml.text "src.git")
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
    <|> Toml.dimatch
      ( \case
          Repology {..} -> Just Repology {..}
          _ -> Nothing
      )
      id
      ( Toml.textBy
          ( \case
              Repology {..} -> repology <> ":" <> repo
              _ -> error "impossible"
          )
          ( \t -> case T.split (== ':') t of
              [repology, repo] -> Right Repology {..}
              _ -> Left "unexpected repology source: it should be something like [project]:[repo]"
          )
          "src.repology"
      )

unsupportError :: a
unsupportError = error "serialization of fetchers is unsupported"

-- | Use it only for deserialization!!!
fetcherCodec :: TomlCodec (Version -> NixFetcher Fresh)
fetcherCodec =
  Toml.textBy
    unsupportError
    ( \t -> case T.split (== '/') t of
        [owner, rest] -> case T.split (== ':') rest of
          [repo, rawV] ->
            Right $ \(coerce -> realV) -> gitHubFetcher (owner, repo) $ coerce $ T.replace "$ver" rawV realV
          [repo] -> Right $ gitHubFetcher (owner, repo)
          _ -> Left "unexpected github fetcher: it should be something like [owner]/[repo] or [owner]/[repo]:[ver]"
        _ -> Left "unexpected github fetcher: it should be something like [owner]/[repo] or [owner]/[repo]:[ver]"
    )
    "fetch.github"
    <|> Toml.textBy
      unsupportError
      ( \t -> case T.split (== ':') t of
          [fpypi, rawV] ->
            Right $ \(coerce -> realV) -> pypiFetcher fpypi $ coerce $ T.replace "$ver" rawV realV
          [fpypi] -> Right $ pypiFetcher fpypi
          _ -> Left "unexpected pypi fetcher: it should be something like [pypi] or [pypi]:[ver]"
      )
      "fetch.pypi"
    <|> Toml.textBy
      unsupportError
      ( \t -> case T.split (== ':') t of
          [furl, rawV] ->
            Right $ \(coerce -> realV) -> gitFetcher furl $ coerce $ T.replace "$ver" rawV realV
          [furl] -> Right $ gitFetcher furl
          _ -> Left "unexpected git fetcher: it should be something like [git_url] or [git_url]:[ver]"
      )
      "fetch.git"
    <|> Toml.textBy
      unsupportError
      (\t -> Right $ \(coerce -> v) -> urlFetcher $ T.replace "$ver" v t)
      "fetch.url"
