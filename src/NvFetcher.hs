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
-- main = runNvFetcher packageSet
--
-- packageSet :: PackageSet ()
-- packageSet = do
--   define $ package "feeluown-core" `fromPypi` "feeluown"
--   define $ package "qliveplayer" `fromGitHub` ("THMonster", "QLivePlayer")
-- @
--
-- You can find more examples of packages in @Main_example.hs@.
--
-- Running the created program:
--
-- * @main@ -- abbreviation of @main build@
-- * @main build@ -- build nix sources expr from given @packageSet@
-- * @main clean@ -- delete .shake dir and generated nix file
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

import Control.Monad.Extra (when, whenJust)
import qualified Data.Map.Strict as Map
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
import Text.Regex.TDFA ((=~))

-- | Arguments for running nvfetcher
data Args = Args
  { -- | Shake options
    argShakeOptions :: ShakeOptions,
    -- | Build target
    argTarget :: String,
    -- | Shake dir
    argBuildDir :: FilePath,
    -- | Custom rules
    argRules :: Rules (),
    -- | Action run after build rule
    argActionAfterBuild :: Action (),
    -- | Action run after clean rule
    argActionAfterClean :: Action (),
    -- | Retry times
    argRetries :: Int,
    -- | Package whose name matches might be updated
    argFilterRegex :: Maybe String
  }

-- | Default arguments of 'defaultMain'
--
-- Build dir is @_sources@.
defaultArgs :: Args
defaultArgs =
  Args
    ( shakeOptions
        { shakeProgress = progressSimple,
          shakeThreads = 0
        }
    )
    "build"
    "_sources"
    (pure ())
    (pure ())
    (pure ())
    3
    Nothing

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
    { argActionAfterBuild = do
        whenJust logPath logChangesToFile
        when commit commitChanges,
      argTarget = target,
      argShakeOptions =
        (argShakeOptions defaultArgs)
          { shakeTimings = timing,
            shakeVerbosity = if verbose then Verbose else Info,
            shakeThreads = threads,
            shakeFiles = buildDir
          },
      argFilterRegex = pkgNameFilter
    }

logChangesToFile :: FilePath -> Action ()
logChangesToFile fp = do
  changes <- getVersionChanges
  writeFile' fp $ unlines $ show <$> changes

commitChanges :: Action ()
commitChanges = do
  changes <- getVersionChanges
  let commitMsg = case changes of
        [x] -> Just $ show x
        xs@(_ : _) -> Just $ "Update\n" <> unlines (show <$> xs)
        [] -> Nothing
  whenJust commitMsg $ \msg -> do
    putInfo "Commiting changes"
    getShakeDir >>= \dir -> command_ [] "git" ["add", dir]
    command_ [] "git" ["commit", "-m", msg]

-- | Entry point of nvfetcher
runNvFetcherNoCLI :: Args -> PackageSet () -> IO ()
runNvFetcherNoCLI args@Args {..} packageSet = do
  pkgs <- Map.map pinIfUnmatch <$> runPackageSet packageSet
  shakeExtras <- initShakeExtras pkgs argRetries
  let opts =
        argShakeOptions
          { shakeExtra = addShakeExtra shakeExtras (shakeExtra argShakeOptions)
          }
      rules = mainRules args
  shake opts $ want [argTarget] >> rules
  where
    -- Don't touch already pinned packages
    pinIfUnmatch x@Package {..}
      | Just regex <- argFilterRegex =
        x
          { _ppinned =
              if coerce _ppinned || not (_pname =~ regex)
                then UseStaleVersion True
                else UseStaleVersion False
          }
      | otherwise = x

--------------------------------------------------------------------------------

mainRules :: Args -> Rules ()
mainRules Args {..} = do
  "clean" ~> do
    getShakeDir >>= flip removeFilesAfter ["//*"]
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
    argActionAfterBuild

  argRules
  coreRules

srouces :: Text -> Text
srouces body =
  [trimming|
    # This file was generated by nvfetcher, please do not modify it manually.
    { fetchgit, fetchurl, fetchFromGitHub }:
    {
      $body
    }
  |]
