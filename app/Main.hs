{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NvFetcher
import NvFetcher.Options
import Options.Applicative.Simple
import qualified Toml

getCLIOptionsWithConfig :: IO (CLIOptions, FilePath)
getCLIOptionsWithConfig =
  getCLIOptions $
    (,) <$> cliOptionsParser
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
  toml <- Toml.parse <$> T.readFile configPath
  case toml of
    Left e -> error $ T.unpack $ Toml.prettyTomlDecodeError $ Toml.ParseError e
    Right x -> case parseConfig x of
      Left e -> error $ T.unpack $ prettyPackageConfigParseError e
      Right pkgs -> runNvFetcherNoCLI (applyCliOptions def opt) (optTarget opt) $ purePackageSet pkgs
