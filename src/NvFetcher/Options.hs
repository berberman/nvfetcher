{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- CLI interface of nvfetcher
module NvFetcher.Options
  ( CLIOptions (..),
    cliOptionsParser,
    getCLIOptions,
  )
where

import Options.Applicative.Simple
import qualified Paths_nvfetcher as Paths

-- | Options for nvfetcher CLI
data CLIOptions = CLIOptions
  { outputPath :: FilePath,
    logPath :: Maybe FilePath,
    threads :: Int,
    retries :: Int,
    timing :: Bool,
    verbose :: Bool,
    target :: String
  }
  deriving (Show)

cliOptionsParser :: Parser CLIOptions
cliOptionsParser =
  CLIOptions
    <$> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Path to output nix file"
          <> showDefault
          <> value "sources.nix"
          <> completer (bashCompleter "file")
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
          <> help "Times to retry of some rules (nvchecker, prefetch, nix-instantiate, etc.)"
          <> value 3
          <> showDefault
      )
    <*> switch (long "timing" <> short 't' <> help "Show build time")
    <*> switch (long "verbose" <> short 'v' <> help "Verbose mode")
    <*> strArgument
      ( metavar "TARGET"
          <> help "Two targets are available: 1.build  2.clean"
          <> value "build"
          <> completer (listCompleter ["build", "clean"])
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
      "nvfetcher - generate nix sources expr for the latest version of packages"
      ( unlines
          [ "It's important to keep _build dir.",
            "If you change any field of an existing package, you may have to run target \"clean\" to invalidate the databse,",
            "making sure the consistency of our build system."
          ]
      )
      parser
      empty
  pure opts
