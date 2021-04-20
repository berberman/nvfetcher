{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Development.NvFetcher.Nvchecker
  ( VersionSource (..),
    nvcheckerRule,
    askVersion,
  )
where

import Control.Monad (void)
import qualified Data.Aeson as A
import Data.Coerce (coerce)
import qualified Data.Text as T
import Development.NvFetcher.Types
import Development.Shake
import NeatInterpolation (trimming)

--------------------------------------------------------------------------------

newtype NvcheckerResult = NvcheckerResult Version

instance A.FromJSON NvcheckerResult where
  parseJSON = A.withObject "NvcheckerResult" $ \o ->
    NvcheckerResult <$> o A..: "version"

--------------------------------------------------------------------------------

nvcheckerRule :: Rules ()
nvcheckerRule = void $
  addOracle $ \q -> withTempFile $ \config -> do
    writeFile' config $ T.unpack $ genNvConfig "pkg" q
    need [config]
    (CmdTime t, Stdout out) <- cmd $ "nvchecker --logger json -c " <> config
    putInfo $ "Finishing running nvchecker for " <> show q <> ", took " <> show t <> "s"
    case A.decode @NvcheckerResult out of
      Just x -> pure $ coerce x
      Nothing -> fail $ "Unable to run nvchecker with: " <> show q
  where
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

askVersion :: VersionSource -> Action Version
askVersion = askOracle
