{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021-2022 berberman
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
  ( runNvFetcher,
    runNvFetcher',
    runNvFetcherNoCLI,
    applyCliOptions,
    parseLastVersions,
    module NvFetcher.PackageSet,
    module NvFetcher.Types,
    module NvFetcher.Types.ShakeExtras,
  )
where

import Control.Monad.Extra (forM_, unless, when, whenJust, whenM)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import NeatInterpolation (trimming)
import NvFetcher.Config
import NvFetcher.Core
import NvFetcher.NixExpr (ToNixExpr (toNixExpr))
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Options
import NvFetcher.PackageSet
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import NvFetcher.Utils (aesonKey, getDataDir)
import qualified System.Directory.Extra as D
import Text.Regex.TDFA ((=~))

-- | Run nvfetcher with CLI options
--
-- This function calls 'runNvFetcherNoCLI', using 'def' 'Config' overridden by 'CLIOptions'.
-- Use this function to create your own Haskell executable program.
runNvFetcher :: PackageSet () -> IO ()
runNvFetcher = runNvFetcher' def

-- | Similar to 'runNvFetcher', but uses custom @config@ instead of 'def' overridden by 'CLIOptions'
runNvFetcher' :: Config -> PackageSet () -> IO ()
runNvFetcher' config packageSet =
  getCLIOptions cliOptionsParser >>= \cli ->
    applyCliOptions config cli >>= \o ->
      runNvFetcherNoCLI o (optTarget cli) packageSet

-- | Apply 'CLIOptions' to 'Config'
applyCliOptions :: Config -> CLIOptions -> IO Config
applyCliOptions config CLIOptions {..} = do
  aKeyfile <- case optKeyfile of
    Just k -> Just <$> D.makeAbsolute k
    _ -> pure Nothing
  pure $
    config
      { buildDir = optBuildDir,
        actionAfterBuild = do
          whenJust optLogPath logChangesToFile
          when optCommit commitChanges
          actionAfterBuild config,
        shakeConfig =
          (shakeConfig config)
            { shakeTimings = optTiming,
              shakeVerbosity = if optVerbose then Verbose else Info,
              shakeThreads = optThreads
            },
        filterRegex = optPkgNameFilter,
        retry = optRetry,
        keyfile = aKeyfile,
        keepOldFiles = optKeepOldFiles
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
runNvFetcherNoCLI :: Config -> Target -> PackageSet () -> IO ()
runNvFetcherNoCLI config@Config {..} target packageSet = do
  pkgs <- Map.map pinIfUnmatch <$> runPackageSet packageSet
  lastVersions <- parseLastVersions $ buildDir </> generatedJsonFileName
  shakeDir <- getDataDir
  -- Set shakeFiles
  let shakeOptions1 = shakeConfig {shakeFiles = shakeDir}
  -- shakeConfig in Config will be shakeOptions1 (not including shake extra)
  shakeExtras <- initShakeExtras (config {shakeConfig = shakeOptions1}) pkgs $ fromMaybe mempty lastVersions
  -- Set shakeExtra
  let shakeOptions2 = shakeOptions1 {shakeExtra = addShakeExtra shakeExtras (shakeExtra shakeConfig)}
      rules = mainRules config
  shake shakeOptions2 $ want [show target] >> rules
  where
    -- Don't touch already pinned packages
    pinIfUnmatch x@Package {..}
      | Just regex <- filterRegex =
          x
            { _ppinned = case _ppinned of
                PermanentStale -> PermanentStale
                _ ->
                  if _pname =~ regex
                    then NoStale
                    else TemporaryStale
            }
      | otherwise = x

--------------------------------------------------------------------------------

mainRules :: Config -> Rules ()
mainRules Config {..} = do
  "clean" ~> do
    getBuildDir >>= flip removeFilesAfter ["//*"]
    actionAfterClean

  "purge" ~> do
    shakeDir <- shakeFiles <$> getShakeOptions
    removeFilesAfter shakeDir ["//*"]

  "build" ~> do
    -- remove all files in build dir except generated nix and json
    -- since core rule has always rerun, any file not generated in this run will be removed
    unless keepOldFiles $
      whenM (liftIO $ D.doesDirectoryExist buildDir) $ do
        oldFiles <- (\\ [generatedJsonFileName, generatedNixFileName]) <$> liftIO (D.listDirectory buildDir)
        putVerbose $ "Removing old files: " <> show oldFiles
        liftIO $ removeFiles buildDir oldFiles
    -- removeFiles buildDir $ oldFiles \\ [generatedNixFileName, generatedJsonFileName]
    allKeys <- getAllPackageKeys
    results <- parallel $ runPackage <$> allKeys
    -- Record removed packages to version changes
    getAllOnDiskVersions
      >>= \oldPkgs -> forM_ (Map.keys oldPkgs \\ allKeys) $
        \pkg -> recordVersionChange (coerce pkg) (oldPkgs Map.!? pkg) "∅"
    getVersionChanges >>= \changes ->
      if null changes
        then putInfo "Up to date"
        else do
          putInfo "Changes:"
          putInfo $ unlines $ show <$> changes
    buildDir <- getBuildDir
    let generatedNixPath = buildDir </> generatedNixFileName
        generatedJSONPath = buildDir </> generatedJsonFileName
    putVerbose $ "Generating " <> generatedNixPath
    writeFileChanged generatedNixPath $ T.unpack $ srouces (T.unlines $ toNixExpr <$> results) <> "\n"
    putVerbose $ "Generating " <> generatedJSONPath
    writeFileChanged generatedJSONPath $ LBS.unpack $ A.encodePretty $ A.object [aesonKey (_prname r) A..= r | r <- results]
    actionAfterBuild

  customRules
  coreRules

srouces :: Text -> Text
srouces body =
  [trimming|
    # This file was generated by nvfetcher, please do not modify it manually.
    { fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
    {
      $body
    }
  |]

generatedNixFileName :: String
generatedNixFileName = "generated.nix"

generatedJsonFileName :: String
generatedJsonFileName = "generated.json"
