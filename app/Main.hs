{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Config
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import NvFetcher
import Options.Applicative.Simple
import qualified Paths_nvfetcher as Paths
import qualified Toml

data CLIOptions = CLIOptions
  { configPath :: FilePath,
    outputPath :: FilePath,
    logPath :: Maybe FilePath,
    threads :: Int,
    timing :: Bool,
    verbose :: Bool,
    target :: String
  }
  deriving (Show)

cliOptionsParser :: Parser CLIOptions
cliOptionsParser =
  CLIOptions
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "Path to nvfetcher TOML config"
          <> value "nvfetcher.toml"
          <> showDefault
          <> completer (bashCompleter "file")
      )
      <*> strOption
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
      <*> ( option
              auto
              ( short 'j'
                  <> metavar "NUM"
                  <> help "Number of threads (0: detected number of processors)"
                  <> value 0
                  <> showDefault
              )
          )
      <*> switch (long "timing" <> short 't' <> help "Show build time")
      <*> switch (long "verbose" <> short 'v' <> help "Verbose mode")
      <*> strArgument
        ( metavar "TARGET"
            <> help "Two targets are available: build and clean"
            <> value "build"
            <> completer (listCompleter ["build", "clean"])
        )

version :: String
version = $(simpleVersion Paths.version)

getCLIOptions :: IO CLIOptions
getCLIOptions = do
  (opts, ()) <-
    simpleOptions
      version
      "nvfetcher - generate nix sources expr for the latest version of packages"
      ( unlines
          [ "It's important to keep .shake dir to make caches work properly.",
            "If you change the version source or fetcher of an existing package, please run target \"clean\" to rebuild everything."
          ]
      )
      cliOptionsParser
      empty
  pure opts

main :: IO ()
main = do
  CLIOptions {..} <- getCLIOptions
  let args =
        defaultArgs
          { argOutputFilePath = outputPath,
            argActionAfterBuild = maybe (pure ()) logChangesToFile logPath,
            argTarget = target,
            argShakeOptions =
              (argShakeOptions defaultArgs)
                { shakeTimings = timing,
                  shakeVerbosity = if verbose then Verbose else Info,
                  shakeThreads = threads
                }
          }
  toml <- Toml.parse <$> T.readFile configPath
  case toml of
    Left e -> error $ T.unpack $ Toml.prettyTomlDecodeError $ Toml.ParseError e
    Right x -> case parseConfig x of
      Left e -> error $ T.unpack $ Toml.prettyTomlDecodeErrors e
      Right pkgs -> runNvFetcher args $ purePackageSet pkgs

logChangesToFile :: FilePath -> Action ()
logChangesToFile fp = do
  changes <- getVersionChanges
  writeFile' fp $ unlines $ show <$> changes
