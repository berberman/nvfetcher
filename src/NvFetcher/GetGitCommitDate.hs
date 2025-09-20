{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021-2025 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module provides mechanisms for obtaining the git commit date.
-- The cloned repo will not be preserved.
module NvFetcher.GetGitCommitDate
  ( -- * Types
    GitDateFormat (..),
    GetGitCommitDate (..),
    GitTimeZone (..),

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
  addOracleCache $ \q@(GetGitCommitDate (T.unpack -> url) (T.unpack -> rev) (coerce -> format, coerce -> tz)) -> withTempDir $ \repo -> do
    putInfo . show $ "#" <+> pretty q
    (StdoutTrim out) <- quietly $ do
      cmd_ [Cwd repo, EchoStderr False, EchoStdout False] ("git init" :: String)
      cmd_ [Cwd repo, EchoStderr False] $ "git remote add origin " <> url
      cmd_ [Cwd repo, EchoStderr False] $ "git fetch --depth 1 origin " <> rev
      cmd_ [Cwd repo, EchoStderr False] ("git checkout FETCH_HEAD" :: String)
      cmd
        ( [Cwd repo, Shell]
            -- If the time zone is not local, set it in the environment
            <> [AddEnv "TZ" tz' | Just (T.unpack -> tz') <- [tz], tz' /= "local"]
        )
        $ "git --no-pager log -1 "
          <> "--format=%cd --date=format"
          -- Use --date-local instead of --date if a time zone is specified
          <> maybe "" (const "-local") tz
          <> ":\""
          <> T.unpack (fromMaybe "%Y-%m-%d" format)
          <> "\""
    pure $ T.pack out

getGitCommitDate :: Text -> Text -> (GitDateFormat, GitTimeZone) -> Action Text
getGitCommitDate url rev = askOracle . GetGitCommitDate url rev
