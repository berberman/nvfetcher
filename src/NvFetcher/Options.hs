{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021-2025 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- CLI interface of nvfetcher
module NvFetcher.Options
  ( CLIOptions (..),
    Target (..),
    cliOptionsParser,
    getCLIOptions,
  )
where

import Options.Applicative.Simple
import qualified Paths_nvfetcher as Paths

data Target = Build | Clean | Purge
  deriving (Eq)

instance Show Target where
  show Build = "build"
  show Clean = "clean"
  show Purge = "purge"

targetParser :: ReadM Target
targetParser = maybeReader $ \case
  "build" -> Just Build
  "clean" -> Just Clean
  "purge" -> Just Purge
  _ -> Nothing

-- | Options for nvfetcher CLI
data CLIOptions = CLIOptions
  { optBuildDir :: FilePath,
    optCommit :: Bool,
    optCommitSummary :: Maybe String,
    optLogPath :: Maybe FilePath,
    optThreads :: Int,
    optRetry :: Int,
    optTiming :: Bool,
    optVerbose :: Bool,
    optPkgNameFilter :: Maybe String,
    optKeyfile :: Maybe FilePath,
    optKeepOldFiles :: Bool,
    optKeepGoing :: Bool,
    optTarget :: Target
  }
  deriving (Show)

cliOptionsParser :: Parser CLIOptions
cliOptionsParser =
  CLIOptions
    <$> strOption
      ( long "build-dir"
          <> short 'o'
          <> metavar "DIR"
          <> help "Directory that nvfetcher puts artifacts to"
          <> showDefault
          <> value "_sources"
          <> completer (bashCompleter "directory")
      )
    <*> switch
      ( long "commit-changes"
          <> help "`git commit` build dir with version changes as commit message"
      )
    <*> optional
      ( strOption
          ( long "commit-summary"
              <> metavar "SUMMARY"
              <> help "Summary to use when committing changes"
          )
      )
    <*> optional
      ( strOption
          ( long "changelog"
              <> short 'l'
              <> metavar "FILE"
              <> help "Dump version changes to a file"
              <> completer (bashCompleter "file")
          )
      )
    <*> option
      auto
      ( short 'j'
          <> metavar "NUM"
          <> help "Number of threads (0: detected number of processors)"
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( short 'r'
          <> long "retry"
          <> metavar "NUM"
          <> help "Times to retry of some rules (nvchecker, prefetch, nix-build, etc.)"
          <> value 3
          <> showDefault
      )
    <*> switch (long "timing" <> short 't' <> help "Show build time")
    <*> switch (long "verbose" <> short 'v' <> help "Verbose mode")
    <*> optional
      ( strOption
          ( short 'f'
              <> long "filter"
              <> metavar "REGEX"
              <> help "Regex to filter packages to be updated"
          )
      )
    <*> optional
      ( strOption
          ( short 'k'
              <> long "keyfile"
              <> metavar "FILE"
              <> help "Nvchecker keyfile"
              <> completer (bashCompleter "file")
          )
      )
    <*> switch (long "keep-old" <> help "Don't remove old files other than generated json and nix before build")
    <*> switch (long "keep-going" <> help "Don't stop if some packages failed to be fetched")
    <*> argument
      targetParser
      ( metavar "TARGET"
          <> help "Three targets are available: 1.build  2.clean (remove all generated files) 3.purge (remove shake db)"
          <> value Build
          <> completer (listCompleter [show Build, show Clean, show Purge])
          <> showDefault
      )

version :: String
version = $(simpleVersion Paths.version)

-- | Parse nvfetcher CLI options
getCLIOptions :: Parser a -> IO a
getCLIOptions parser = do
  (opts, ()) <-
    simpleOptions
      version
      "nvfetcher"
      "generate nix sources expr for the latest version of packages"
      parser
      empty
  pure opts
