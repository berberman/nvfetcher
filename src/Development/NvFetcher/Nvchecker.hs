{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.Text as T
import Development.NvFetcher.Types
import Development.Shake
import Development.Shake.Rule
import NeatInterpolation (trimming)

nvcheckerRule :: Rules ()
nvcheckerRule = addBuiltinRule noLint noIdentity $ \q old _mode -> withTempFile $ \config -> do
  writeFile' config $ T.unpack $ genNvConfig "pkg" q
  need [config]
  (CmdTime t, Stdout out) <- quietly $ cmd $ "nvchecker --logger json -c " <> config
  putInfo $ "Finishing running nvchecker for " <> show q <> ", took " <> show t <> "s"
  now <- case A.decode @NvcheckerResult out of
    Just x -> pure x
    Nothing -> fail $ "Unable to run nvchecker with: " <> show q
  -- Try to delegate nvtake's work, i.e. saving the last version to produce changelog
  -- Not sure if this works
  case old of
    Just lastRun
      | cachedResult <- decode' lastRun ->
        if cachedResult == nvNow now
          then pure $ RunResult ChangedRecomputeSame lastRun now
          else pure $ RunResult ChangedRecomputeDiff (encode' $ nvNow now) now {nvOld = Just cachedResult}
    Nothing -> pure $ RunResult ChangedRecomputeDiff (encode' $ nvNow now) now
  where
    encode' :: Binary a => a -> BS.ByteString
    encode' = BS.concat . LBS.toChunks . encode
    decode' = decode . LBS.fromChunks . pure
    genNvConfig srcName = \case
      GitHub {..} ->
        [trimming|
              [$srcName]
              source = "github"
              github = "$owner/$repo"
              use_latest_release = true
        |]
      Aur {..} ->
        [trimming|
              [$srcName]
              source = "aur"
              archpkg = "$aur"
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
