{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021-2022 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module provides function to calculate @cargoLock@ used in @rustPlatform.buildRustPackage@.
module NvFetcher.FetchRustGitDeps
  ( -- * Types
    FetchRustGitDepsQ (..),

    -- * Rules
    fetchRustGitDepsRule,

    -- * Functions
    fetchRustGitDeps,
  )
where

import Control.Monad (void)
import Data.Binary.Instances ()
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.List.Extra (nubOrdOn)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import NvFetcher.ExtractSrc
import NvFetcher.NixFetcher
import NvFetcher.Types
import Prettyprinter (pretty, (<+>))
import Text.Parsec
import Text.Parsec.Text
import Toml (TomlCodec, (.=))
import qualified Toml

-- | Rules of fetch rust git dependencies
fetchRustGitDepsRule :: Rules ()
fetchRustGitDepsRule = void $
  addOracleCache $ \key@(FetchRustGitDepsQ fetcher lockPath) -> do
    putInfo . show $ "#" <+> pretty key
    cargoLock <- head . HMap.elems <$> extractSrc fetcher lockPath
    deps <- case Toml.decode (Toml.list rustDepCodec "package") cargoLock of
      Right r -> pure $ nubOrdOn rrawSrc r
      Left err -> fail $ "Failed to parse Cargo.lock: " <> T.unpack (Toml.prettyTomlDecodeErrors err)
    r <-
      parallel
        [ case parse gitSrcParser (T.unpack rname) src of
            Right ParsedGitSrc {..} -> do
              (_sha256 -> sha256) <- prefetch (gitFetcher pgurl pgsha) NoForceFetch
              -- @${name}-${version}@ -> sha256
              pure (rname <> "-" <> coerce rversion, sha256)
            Left err -> fail $ "Failed to parse git source in Cargo.lock: " <> show err
          | RustDep {..} <- deps,
            -- it's a dependency
            src <- maybeToList rrawSrc,
            -- it's a git dependency
            "git+" `T.isPrefixOf` src
        ]
    pure $ HMap.fromList r

-- | Run fetch rust git dependencies
fetchRustGitDeps ::
  -- | prefetched source
  NixFetcher Fetched ->
  -- | relative file path of @Cargo.lock@
  FilePath ->
  Action (HashMap Text Checksum)
fetchRustGitDeps fetcher lockPath = askOracle $ FetchRustGitDepsQ fetcher lockPath

data ParsedGitSrc = ParsedGitSrc
  { -- | git url
    pgurl :: Text,
    pgsha :: Version
  }
  deriving (Show, Eq, Ord)

-- | Parse git src in cargo lock file
-- >>> parse gitSrcParser "test" "git+https://github.com/rust-random/rand.git?rev=0.8.3#6ecbe2626b2cc6110a25c97b1702b347574febc7"
-- Right (ParsedGitSrc {pgurl = "https://github.com/rust-random/rand.git", pgsha = "6ecbe2626b2cc6110a25c97b1702b347574febc7"})
--
-- >>> parse gitSrcParser "test" "git+https://github.com/rust-random/rand.git#f0e01ee0a7257753cc51b291f62666f4765923ef"
-- Right (ParsedGitSrc {pgurl = "https://github.com/rust-random/rand.git", pgsha = "f0e01ee0a7257753cc51b291f62666f4765923ef"})
--
-- >>> parse gitSrcParser "test" "git+https://github.com/rust-lang/cargo?branch=rust-1.53.0#4369396ce7d270972955d876eaa4954bea56bcd9"
-- Right (ParsedGitSrc {pgurl = "https://github.com/rust-lang/cargo", pgsha = "4369396ce7d270972955d876eaa4954bea56bcd9"})
gitSrcParser :: Parser ParsedGitSrc
gitSrcParser = do
  _ <- string "git+"
  pgurl <- many1 $ noneOf ['?', '#']
  -- skip things like ?rev and ?branch
  skipMany (noneOf ['#'])
  _ <- char '#'
  pgsha <- manyTill anyChar eof
  pure $ ParsedGitSrc (T.pack pgurl) (coerce $ T.pack pgsha)

data RustDep = RustDep
  { rname :: PackageName,
    rversion :: Version,
    rrawSrc :: Maybe Text
  }
  deriving (Show, Eq, Ord)

rustDepCodec :: TomlCodec RustDep
rustDepCodec =
  RustDep
    <$> Toml.text "name" .= rname
    <*> Toml.diwrap (Toml.text "version") .= rversion
    <*> Toml.dioptional (Toml.text "source") .= rrawSrc
