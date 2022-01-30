{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    runNvFetcher',
    runNvFetcherNoCLI,
    applyCliOptions,
    parseLastVersions,
    module NvFetcher.PackageSet,
    module NvFetcher.Types,
    module NvFetcher.Types.ShakeExtras,
  )
where

import Control.Monad.Extra (forM_, when, whenJust)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import NeatInterpolation (trimming)
import NvFetcher.Core
import NvFetcher.NixExpr (ToNixExpr (toNixExpr))
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Options
import NvFetcher.PackageSet
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils (getDataDir)
import qualified System.Directory.Extra as D
import Text.Regex.TDFA ((=~))

-- | Arguments for running nvfetcher
data Args = Args
  { -- | Shake options
    argShakeOptions :: ShakeOptions,
    -- | Build target
    argTarget :: String,
    -- | Build dir
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

-- | Default 'Args'
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
runNvFetcher = runNvFetcher' defaultArgs

-- | Similar to 'runNvFetcher', but uses custom @args@ instead of 'defaultArgs'
runNvFetcher' :: Args -> PackageSet () -> IO ()
runNvFetcher' args packageSet =
  getCLIOptions cliOptionsParser >>= flip runNvFetcherNoCLI packageSet . applyCliOptions args

-- | Apply 'CLIOptions' to 'Args'
applyCliOptions :: Args -> CLIOptions -> Args
applyCliOptions args CLIOptions {..} =
  args
    { argActionAfterBuild = do
        whenJust logPath logChangesToFile
        when commit commitChanges
        argActionAfterBuild args,
      argTarget = target,
      argShakeOptions =
        (argShakeOptions defaultArgs)
          { shakeTimings = timing,
            shakeVerbosity = if verbose then Verbose else Info,
            shakeThreads = threads
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
    getBuildDir >>= \dir -> command_ [] "git" ["add", dir]
    command_ [] "git" ["commit", "-m", msg]

-- | @Parse generated.nix@
parseLastVersions :: FilePath -> IO (Maybe (Map.Map PackageKey Version))
parseLastVersions jsonFile =
  D.doesFileExist jsonFile >>= \case
    True -> do
      objs <- A.decodeFileStrict' jsonFile
      pure $
        flip fmap objs $
          ( \xs ->
              Map.fromList
                . catMaybes
                $ [(PackageKey k,) <$> A.parseMaybe (A..: "version") obj | (k, obj) <- xs]
          )
            . Map.toList
    _ -> pure mempty

-- | Entry point of nvfetcher
runNvFetcherNoCLI :: Args -> PackageSet () -> IO ()
runNvFetcherNoCLI args@Args {..} packageSet = do
  pkgs <- Map.map pinIfUnmatch <$> runPackageSet packageSet
  lastVersions <- parseLastVersions $ argBuildDir </> "generated.json"
  shakeExtras <- initShakeExtras pkgs argRetries argBuildDir $ fromMaybe mempty lastVersions
  shakeDir <- getDataDir
  let opts =
        argShakeOptions
          { shakeFiles = shakeDir,
            shakeExtra = addShakeExtra shakeExtras (shakeExtra argShakeOptions)
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
    getBuildDir >>= flip removeFilesAfter ["//*"]
    argActionAfterClean

  "build" ~> do
    allKeys <- getAllPackageKeys
    results <- parallel $ runPackage <$> allKeys
    -- Record removed packages to version changes
    getShakeExtras
      >>= ( \oldPkgs -> forM_ (Map.keys oldPkgs \\ allKeys) $
              \pkg -> recordVersionChange (coerce pkg) (oldPkgs Map.!? pkg) "∅"
          )
        . lastVersions
    getVersionChanges >>= \changes ->
      if null changes
        then putInfo "Up to date"
        else do
          putInfo "Changes:"
          putInfo $ unlines $ show <$> changes
    buildDir <- getBuildDir
    let generatedNixPath = buildDir </> "generated.nix"
        generatedJSONPath = buildDir </> "generated.json"
    putVerbose $ "Generating " <> generatedNixPath
    writeFileChanged generatedNixPath $ T.unpack $ srouces (T.unlines $ toNixExpr <$> results) <> "\n"
    putVerbose $ "Generating " <> generatedJSONPath
    writeFileChanged generatedJSONPath $ LBS.unpack $ A.encodePretty $ A.object [_prname r A..= r | r <- results]
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
