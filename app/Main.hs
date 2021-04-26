{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Config
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.NvFetcher
import System.Console.GetOpt
import qualified Toml

data CLIArgs = CLIArgs
  { configPath :: FilePath,
    outputPath :: FilePath
  }
  deriving (Show)

defaultCLIArgs :: CLIArgs
defaultCLIArgs = CLIArgs "nvfetcher.toml" "sources.nix"

flags :: [OptDescr (Either String (CLIArgs -> CLIArgs))]
flags =
  [ Option ['c'] ["config"] (ReqArg (\s -> Right $ \o -> o {configPath = s}) "FILE") "Path to nvfetcher config",
    Option ['o'] ["output"] (ReqArg (\s -> Right $ \o -> o {outputPath = s}) "FILE") "Path to output nix file"
  ]

main :: IO ()
main = void $
  defaultMainWith flags $ \flagValues -> do
    let CLIArgs {..} = foldl (flip id) defaultCLIArgs flagValues
    toml <- Toml.parse <$> T.readFile configPath
    case toml of
      Left e -> error $ T.unpack $ Toml.prettyTomlDecodeError $ Toml.ParseError e
      Right x -> case parseConfig x of
        Left e -> error $ T.unpack $ Toml.prettyTomlDecodeErrors e
        Right x -> pure (defaultArgs {argOutputFilePath = outputPath}, purePackageSet x)
