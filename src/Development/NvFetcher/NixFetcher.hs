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
    gitFetcher,
    urlFetcher,
    prefetch,
  )
where

import Control.Monad (void, (<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Coerce (coerce)
import Data.Maybe (maybeToList)
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

instance ToNixExpr Bool where
  toNixExpr True = "true"
  toNixExpr False = "false"

instance ToNixExpr Version where
  toNixExpr = coerce

class ToPrefetchCommand a where
  toPrefetchCommand :: a -> Action SHA256

instance ToPrefetchCommand (NixFetcher Fresh) where
  toPrefetchCommand = \case
    g@FetchGit {..} -> do
      let parser = A.withObject "nix-prefetch-git" $ \o -> SHA256 <$> o A..: "sha256"
      (CmdTime t, Stdout (A.parseMaybe parser <=< A.decode -> out)) <-
        command [EchoStderr False] "nix-prefetch-git" $
          [T.unpack furl]
            <> ["--rev", T.unpack $ coerce rev]
            <> ["--fetch-submodules" | fetchSubmodules]
            <> ["--branch-name " <> T.unpack b | b <- maybeToList branch]
            <> ["--deepClone" | deepClone]
            <> ["--leave-dotGit" | leaveDotGit]
      putInfo $ "Finishing prefetching, took " <> show t <> "s"
      case out of
        Just x -> pure x
        _ -> fail $ "Failed to prefetch: " <> show g
    g@FetchUrl {..} -> do
      (CmdTime t, Stdout (T.decodeUtf8 -> out)) <- command [EchoStderr False] "nix-prefetch-url" [T.unpack furl]
      putInfo $ "Finishing prefetching, took " <> show t <> "s"
      case takeWhile (not . T.null) $ reverse $ T.lines out of
        [x] -> pure $ coerce x
        _ -> fail $ "Failed to prefetch: " <> show g

buildNixFetcher :: Text -> NixFetcher k -> Text
buildNixFetcher sha256 = \case
  FetchGit
    { sha256 = _,
      rev = toNixExpr -> rev,
      fetchSubmodules = toNixExpr -> fetchSubmodules,
      deepClone = toNixExpr -> deepClone,
      leaveDotGit = toNixExpr -> leaveDotGit,
      ..
    } ->
      [trimming|
          fetchgit {
            url = "$furl";
            rev = "$rev";
            fetchSubmodules = $fetchSubmodules;
            deepClone = $deepClone;
            leaveDotGit = $leaveDotGit;
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

pypiUrl :: Text -> Version -> Text
pypiUrl pypi (coerce -> ver) =
  let h = T.cons (T.head pypi) ""
   in [trimming|https://pypi.io/packages/source/$h/$pypi/$pypi-$ver.tar.gz|]

--------------------------------------------------------------------------------

prefetchRule :: Rules ()
prefetchRule = void $
  addOracleCache $ \(f :: NixFetcher Fresh) -> do
    sha256 <- toPrefetchCommand f
    pure $ f {sha256 = sha256}

--------------------------------------------------------------------------------

gitFetcher :: Text -> Version -> NixFetcher Fresh
gitFetcher furl rev = FetchGit furl rev Nothing False False False ()

gitHubFetcher :: (Text, Text) -> Version -> NixFetcher Fresh
gitHubFetcher (owner, repo) = gitFetcher [trimming|https://github.com/$owner/$repo|]

pypiFetcher :: Text -> Version -> NixFetcher Fresh
pypiFetcher p v = urlFetcher $ pypiUrl p v

gitHubReleaseFetcher :: (Text, Text) -> Text -> Version -> NixFetcher Fresh
gitHubReleaseFetcher (owner, repo) fp (coerce -> ver) =
  urlFetcher
    [trimming|https://github.com/$owner/$repo/releases/download/$ver/$fp|]

urlFetcher :: Text -> NixFetcher Fresh
urlFetcher = flip FetchUrl ()

prefetch :: NixFetcher Fresh -> Action (NixFetcher Prefetched)
prefetch = askOracle
