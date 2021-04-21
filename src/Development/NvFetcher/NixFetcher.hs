{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Development.NvFetcher.NixFetcher
  ( NixFetcher (..),
    Prefetch (..),
    ToNixExpr (..),
    prefetchRule,
    gitHubFetcher,
    pypiFetcher,
    gitHubReleaseFetcher,
    urlFetcher,
    prefetch,
  )
where

import Control.Monad (void, (<=<))
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.NvFetcher.Types
import Development.Shake
import NeatInterpolation (trimming)

--------------------------------------------------------------------------------

class ToNixExpr a where
  toNixExpr :: a -> Text

instance ToNixExpr (NixFetcher Fresh) where
  toNixExpr = buildNixFetcher "lib.fakeSha256"

instance ToNixExpr (NixFetcher Prefetched) where
  -- add quotation marks
  toNixExpr f = buildNixFetcher (T.pack $ show $ T.unpack $ coerce $ sha256 f) f

buildNixFetcher :: Text -> NixFetcher k -> Text
buildNixFetcher sha256 = \case
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
  (FetchPypi pypi (coerce -> ver) _) ->
    let h = T.cons (T.head pypi) ""
     in [trimming|
          fetchurl {
            sha256 = $sha256;
            url = "mirror://pypi/$h/$pypi/$pypi-$ver.tar.gz";
          }
    |]

--------------------------------------------------------------------------------

prefetchRule :: Rules ()
prefetchRule = void $
  addOracleCache $ \(f :: NixFetcher Fresh) -> do
    (CmdTime t, Stdout (T.decodeUtf8 -> out)) <- quietly $ command [] "nix-prefetch" [T.unpack (toNixExpr f)]
    putInfo $ "Finishing prefetching " <> show f <> ", took " <> show t <> "s"
    case (T.stripPrefix "sha256-" <=< lastMaybe . T.lines) out of
      Just sha256 -> pure $ f {sha256 = SHA256 sha256}
      _ -> fail $ "Unable to prefetch " <> show f
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

--------------------------------------------------------------------------------

gitHubFetcher :: (Text, Text) -> Version -> NixFetcher Fresh
gitHubFetcher (fgitHubOwner, fgitHubRepo) fgitHubRev = FetchFromGitHub {..}
  where
    sha256 = ()

pypiFetcher :: Text -> Version -> NixFetcher Fresh
pypiFetcher fpypi fpypiV = FetchPypi {..}
  where
    sha256 = ()

gitHubReleaseFetcher :: (Text, Text) -> Text -> Version -> NixFetcher Fresh
gitHubReleaseFetcher (owner, repo) fp (coerce -> ver) =
  FetchUrl [trimming|https://github.com/$owner/$repo/releases/download/$ver/$fp|] ()

urlFetcher :: Text -> NixFetcher Fresh
urlFetcher = flip FetchUrl ()

prefetch :: NixFetcher Fresh -> Action (NixFetcher Prefetched)
prefetch = askOracle
