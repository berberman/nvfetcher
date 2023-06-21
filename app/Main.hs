{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NvFetcher
import NvFetcher.Options
import Options.Applicative.Simple

getCLIOptionsWithConfig :: IO (CLIOptions, FilePath)
getCLIOptionsWithConfig =
  getCLIOptions $
    (,)
      <$> cliOptionsParser
      <*> strOption
        ( long "config"
            <> short 'c'
            <> metavar "FILE"
            <> help "Path to nvfetcher TOML config"
            <> value "nvfetcher.toml"
            <> showDefault
            <> completer (bashCompleter "file")
        )

main :: IO ()
main = do
  (opt, configPath) <- getCLIOptionsWithConfig
  raw <- T.readFile configPath
  case parseConfig raw of
    Left e -> error $ T.unpack $ T.unlines $ prettyPackageConfigParseError <$> e
    Right pkgs -> applyCliOptions def opt >>= \o -> runNvFetcherNoCLI o (optTarget opt) $ purePackageSet pkgs
