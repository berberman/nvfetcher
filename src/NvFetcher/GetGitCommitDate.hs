{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021-2022 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module provides mechanisms for obtaining the git commit date.
-- The cloned repo will not be preserved.
module NvFetcher.GetGitCommitDate
  ( -- * Types
    DateFormat (..),
    GetGitCommitDate (..),

    -- * Rules
    getGitCommitDateRule,

    -- * Functions
    getGitCommitDate,
  )
where

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import NvFetcher.Types
import Prettyprinter (pretty, (<+>))

getGitCommitDateRule :: Rules ()
getGitCommitDateRule = void $ do
  addOracleCache $ \q@(GetGitCommitDate (T.unpack -> url) (T.unpack -> rev) format) -> withTempDir $ \repo -> do
    putInfo . show $ "#" <+> pretty q
    (StdoutTrim out) <- quietly $ do
      cmd_ [Cwd repo, EchoStderr False, EchoStdout False] ("git init" :: String)
      cmd_ [Cwd repo, EchoStderr False] $ "git remote add origin " <> url
      cmd_ [Cwd repo, EchoStderr False] $ "git fetch --depth 1 origin " <> rev
      cmd_ [Cwd repo, EchoStderr False] ("git checkout FETCH_HEAD" :: String)
      cmd [Cwd repo, Shell] $ "git --no-pager log -1 --format=%cd --date=format:\"" <> T.unpack (fromMaybe "%Y-%m-%d" $ coerce format) <> "\""
    pure $ T.pack out

getGitCommitDate :: Text -> Text -> DateFormat -> Action Text
getGitCommitDate url rev format = askOracle $ GetGitCommitDate url rev format
