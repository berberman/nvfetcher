{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Development.NvFetcher.Nvchecker
  ( VersionSource (..),
    nvcheckerRule,
    askNvchecker,
  )
where

import qualified Data.Aeson as A
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.NvFetcher.Types
import Development.Shake
import Development.Shake.Rule
import NeatInterpolation (trimming)

nvcheckerRule :: Rules ()
nvcheckerRule = addBuiltinRule noLint noIdentity $ \q old _mode -> withTempFile $ \config -> do
  writeFile' config $ T.unpack $ genNvConfig "pkg" q
  need [config]
  (CmdTime t, Stdout (T.lines . T.decodeUtf8 -> out)) <- cmd $ "nvchecker --logger json -c " <> config
  putInfo $ "Finishing running nvchecker, took " <> show t <> "s"
  let result = catMaybes $ A.decodeStrict . T.encodeUtf8 <$> out
  now <- case result of
    [x] -> pure x
    _ -> fail $ "Unable to run nvchecker with: " <> show q
  pure $ case old of
    Just lastRun
      | cachedResult <- decode' lastRun ->
        if cachedResult == nvNow now
          then -- try to get the version in last run from store, filling it into 'now'
            RunResult ChangedRecomputeSame lastRun now {nvOld = Just cachedResult}
          else RunResult ChangedRecomputeDiff (encode' $ nvNow now) now {nvOld = Just cachedResult}
    Nothing -> RunResult ChangedRecomputeDiff (encode' $ nvNow now) now
  where
    encode' :: Binary a => a -> BS.ByteString
    encode' = BS.concat . LBS.toChunks . encode
    decode' = decode . LBS.fromChunks . pure
    genNvConfig srcName = \case
      GitHubRelease {..} ->
        [trimming|
              [$srcName]
              source = "github"
              github = "$owner/$repo"
              use_latest_release = true
        |]
      Git {..} ->
        [trimming|
              [$srcName]
              source = "git"
              git = "$vurl"
              use_commit = true
        |]
      Aur {..} ->
        [trimming|
              [$srcName]
              source = "aur"
              aur = "$aur"
              strip_release = true
        |]
      ArchLinux {..} ->
        [trimming|
              [$srcName]
              source = "archpkg"
              archpkg = "$archpkg"
              strip_release = true
        |]
      Pypi {..} ->
        [trimming|
              [$srcName]
              source = "pypi"
              pypi = "$pypi"
        |]
      Manual {..} ->
        [trimming|
              [$srcName]
              source = "manual"
              manual = "$manual"
        |]

askNvchecker :: VersionSource -> Action NvcheckerResult
askNvchecker = apply1
