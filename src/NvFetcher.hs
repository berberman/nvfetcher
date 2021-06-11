{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- The main module of nvfetcher. If you want to create CLI program with it, it's enough to import only this module.
--
-- Example:
--
-- @
-- module Main where
--
-- import NvFetcher
--
-- main :: IO ()
-- main = runNvFetcher defaultArgs packageSet
--
-- packageSet :: PackageSet ()
-- packageSet = do
--   define $ package "feeluown-core" `fromPypi` "feeluown"
--   define $ package "qliveplayer" `fromGitHub` ("IsoaSFlus", "QLivePlayer")
-- @
--
-- You can find more examples of packages in @Main_example.hs@.
--
-- Running the created program:
--
-- * @main@ -- abbreviation of @main build@
-- * @main build@ -- build nix sources expr from given @packageSet@
-- * @main clean@ -- delete .shake dir and generated nix file
-- * @main -j@ -- build with parallelism
--
-- All shake options are inherited.
module NvFetcher
  ( Args (..),
    defaultArgs,
    runNvFetcher,
    runNvFetcherNoCLI,
    cliOptionsToArgs,
    module NvFetcher.PackageSet,
    module NvFetcher.Types,
    module NvFetcher.Types.ShakeExtras,
  )
where

import qualified Control.Exception as CE
import Control.Monad.Extra (whenJust)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import NeatInterpolation (trimming)
import NvFetcher.Core
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Options
import NvFetcher.PackageSet
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils (getShakeDir)
import System.Directory.Extra (createDirectoryIfMissing, createFileLink, removeFile)

-- | Arguments for running nvfetcher
data Args = Args
  { -- | Shake options
    argShakeOptions :: ShakeOptions,
    -- | Build target
    argTarget :: String,
    -- | Output file path (the symlink target of @generated.nix@)
    argOutputFilePath :: Maybe FilePath,
    -- | Custom rules
    argRules :: Rules (),
    -- | Action run after build rule
    argActionAfterBuild :: Action (),
    -- | Action run after clean rule
    argActionAfterClean :: Action (),
    -- | Retry times
    argRetries :: Int
  }

-- | Default arguments of 'defaultMain'
--
-- Output file path is @sources.nix@.
defaultArgs :: Args
defaultArgs =
  Args
    ( shakeOptions
        { shakeProgress = progressSimple,
          shakeThreads = 0
        }
    )
    "build"
    (Just "sources.nix")
    (pure ())
    (pure ())
    (pure ())
    3

-- | Run nvfetcher with CLI options
--
-- This function calls 'runNvFetcherNoCLI', using 'Args' from 'CLIOptions'.
-- Use this function to create your own Haskell executable program.
runNvFetcher :: PackageSet () -> IO ()
runNvFetcher packageSet =
  getCLIOptions cliOptionsParser >>= flip runNvFetcherNoCLI packageSet . cliOptionsToArgs

-- | Apply 'CLIOptions' to 'defaultArgs'
cliOptionsToArgs :: CLIOptions -> Args
cliOptionsToArgs CLIOptions {..} =
  defaultArgs
    { argOutputFilePath = if noOutput then Nothing else Just outputPath,
      argActionAfterBuild = maybe (pure ()) logChangesToFile logPath,
      argTarget = target,
      argShakeOptions =
        (argShakeOptions defaultArgs)
          { shakeTimings = timing,
            shakeVerbosity = if verbose then Verbose else Info,
            shakeThreads = threads
          }
    }

logChangesToFile :: FilePath -> Action ()
logChangesToFile fp = do
  changes <- getVersionChanges
  writeFile' fp $ unlines $ show <$> changes

-- | Entry point of nvfetcher
runNvFetcherNoCLI :: Args -> PackageSet () -> IO ()
runNvFetcherNoCLI args@Args {..} packageSet = do
  pkgs <- runPackageSet packageSet
  shakeExtras <- initShakeExtras pkgs argRetries
  let opts =
        argShakeOptions
          { shakeExtra = addShakeExtra shakeExtras (shakeExtra argShakeOptions),
            shakeFiles = "_build"
          }
      rules = mainRules args
  shake opts $ want [argTarget] >> rules

--------------------------------------------------------------------------------

mainRules :: Args -> Rules ()
mainRules Args {..} = do
  "clean" ~> do
    removeFilesAfter "_build" ["//*"]
    whenJust argOutputFilePath $ \out -> removeFilesAfter "." [out]
    argActionAfterClean

  "build" ~> do
    allKeys <- getAllPackageKeys
    body <- parallel $ generateNixSourceExpr <$> allKeys
    getVersionChanges >>= \changes ->
      if null changes
        then putInfo "Up to date"
        else do
          putInfo "Changes:"
          putInfo $ unlines $ show <$> changes
    shakeDir <- getShakeDir
    let genPath = shakeDir </> "generated.nix"
    putVerbose $ "Generating " <> genPath
    writeFileChanged genPath $ T.unpack $ srouces (T.unlines body) <> "\n"
    need [genPath]
    whenJust argOutputFilePath $ \out -> do
      let outDir = takeDirectory out
      liftIO $ createDirectoryIfMissing True outDir
      liftIO $ CE.catch @IOError (removeFile out) (const $ pure ())
      putVerbose $ "Symlinking " <> out
      liftIO $ createFileLink genPath out
    argActionAfterBuild

  argRules
  coreRules

srouces :: Text -> Text
srouces body =
  [trimming|
    # This file was generated by nvfetcher, please do not modify it manually.
    { fetchgit, fetchurl }:
    {
      $body
    }
  |]
