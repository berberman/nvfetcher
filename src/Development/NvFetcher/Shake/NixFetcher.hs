{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Development.NvFetcher.Shake.NixFetcher
  ( NixFetcher (..),
    Prefetch (..),
    prefetchRule,
    prefetchGitHub,
    prefetchPypi,
    prefetchGitHubRelease,
    prefetchUrl,
  )
where

import Control.Monad (void, (<=<))
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.NvFetcher.Types
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import NeatInterpolation (trimming)

data NixFetcher (k :: Prefetch)
  = FetchFromGitHub
      { fetchGitHubOwner :: Text,
        fetchGitHubRepo :: Text,
        fetchGitHubRev :: Version,
        sha256 :: MapPrefetch k
      }
  | FetchUrl {fetchUrl :: Text, sha256 :: MapPrefetch k}
  deriving (Typeable, Generic)

data Prefetch = Fresh | Prefetched

type family MapPrefetch (k :: Prefetch) where
  MapPrefetch Fresh = ()
  MapPrefetch Prefetched = SHA256

type instance RuleResult (NixFetcher Fresh) = NixFetcher Prefetched

deriving instance Show (MapPrefetch k) => Show (NixFetcher k)

deriving instance Eq (MapPrefetch k) => Eq (NixFetcher k)

deriving instance Hashable (MapPrefetch k) => Hashable (NixFetcher k)

deriving instance Binary (MapPrefetch k) => Binary (NixFetcher k)

deriving instance NFData (MapPrefetch k) => NFData (NixFetcher k)

prefetchRule :: Rules ()
prefetchRule = void $
  addOracleCache $ \(f :: NixFetcher Fresh) -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out)) <- command [] "nix-prefetch" [T.unpack (genNixExpr f)]
    putInfo $ "Prefetching " <> show f <> " took " <> show t <> "s"
    case (T.stripPrefix "sha256-" <=< lastMaybe . T.lines) out of
      Just sha256 -> pure $ f {sha256 = SHA256 sha256}
      _ -> fail $ "Unable to prefetch " <> show f
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs
    sha256 = "lib.fakeSha256"
    genNixExpr = \case
      (FetchFromGitHub owner repo (coerce -> rev) _) ->
        [trimming| 
          fetchFromGitHub {
            owner = "$owner";
            repo = "$repo";
            rev = "$rev";
            fetchSubmodules = true;
            sha256 = $sha256;
          }
        |]
      (FetchUrl url _) ->
        [trimming|
          fetchurl {
            sha256 = $sha256;
            url = "$url";
          }
        |]

prefetchGitHub :: (Text, Text) -> Version -> Action (NixFetcher Prefetched)
prefetchGitHub (owner, repo) ver = askOracle @(NixFetcher Fresh) $ FetchFromGitHub owner repo ver ()

prefetchPypi :: Text -> Version -> Action (NixFetcher Prefetched)
prefetchPypi pypi (coerce -> ver) =
  let h = T.cons (T.head pypi) ""
   in askOracle @(NixFetcher Fresh) $ FetchUrl [trimming|mirror://pypi/$h/$pypi/$pypi-$ver.tar.gz|] ()

prefetchGitHubRelease :: (Text, Text) -> Text -> Version -> Action (NixFetcher Prefetched)
prefetchGitHubRelease (owner, repo) fp (coerce -> ver) =
  askOracle @(NixFetcher Fresh) $
    FetchUrl [trimming|https://github.com/$owner/$repo/releases/download/$ver/$fp|] ()

prefetchUrl :: Text -> Action (NixFetcher Prefetched)
prefetchUrl = askOracle @(NixFetcher Fresh) . flip FetchUrl ()
