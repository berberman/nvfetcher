{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Config
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Development.Shake
import NvFetcher
import Paths_nvfetcher
import System.Console.GetOpt
import qualified Toml

data CLIArgs = CLIArgs
  { configPath :: FilePath,
    outputPath :: FilePath,
    versionMode :: Bool,
    logPath :: Maybe FilePath
  }
  deriving (Show)

defaultCLIArgs :: CLIArgs
defaultCLIArgs = CLIArgs "nvfetcher.toml" "sources.nix" False Nothing

flags :: [OptDescr (Either String (CLIArgs -> CLIArgs))]
flags =
  [ Option ['c'] ["config"] (ReqArg (\s -> Right $ \o -> o {configPath = s}) "FILE") "Path to nvfetcher config",
    Option ['o'] ["output"] (ReqArg (\s -> Right $ \o -> o {outputPath = s}) "FILE") "Path to output nix file",
    Option ['v'] ["version"] (NoArg $ Right $ \o -> o {versionMode = True}) "Print nvfetcher version",
    Option ['l'] ["log"] (ReqArg (\s -> Right $ \o -> o {logPath = Just s}) "FILE") "Path to log file"
  ]

logChangesToFile :: FilePath -> Action ()
logChangesToFile fp = do
  changes <- getVersionChanges
  writeFile' fp $ unlines $ show <$> changes

main :: IO ()
main = void $
  defaultMainWith flags $ \flagValues -> do
    let CLIArgs {..} = foldl (flip id) defaultCLIArgs flagValues
    if versionMode
      then putStrLn ("nvfetcher " <> showVersion version) >> pure Nothing
      else do
        toml <- Toml.parse <$> T.readFile configPath
        case toml of
          Left e -> error $ T.unpack $ Toml.prettyTomlDecodeError $ Toml.ParseError e
          Right x -> case parseConfig x of
            Left e -> error $ T.unpack $ Toml.prettyTomlDecodeErrors e
            Right x ->
              pure $
                Just
                  ( defaultArgs
                      { argOutputFilePath = outputPath,
                        argActionAfterBuild = case logPath of
                          Just fp -> logChangesToFile fp
                          _ -> pure ()
                      },
                    purePackageSet x
                  )
