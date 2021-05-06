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
-- main = defaultMain defaultArgs packageSet
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
  ( -- | Re-export DSL things
    module NvFetcher.PackageSet,
    module NvFetcher.Types,
    generateNixSources,
    Args (..),
    defaultArgs,
    runNvFetcher,
    runNvFetcherWith,
    VersionChange (..),
    getVersionChanges,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes (hashWithSalt)
import NeatInterpolation (trimming)
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.PackageSet
import NvFetcher.Types
import System.Console.GetOpt (OptDescr)

-- | Arguments for running nvfetcher
data Args = Args
  { -- | tweak shake options
    argShakeOptions :: ShakeOptions -> ShakeOptions,
    -- | Output file path
    argOutputFilePath :: FilePath,
    -- | Custom rules
    argRules :: Rules (),
    -- | Action run after build rule
    argActionAfterBuild :: Action (),
    -- | Action run after clean rule
    argActionAfterClean :: Action (),
    -- | Input packages
    argPackageSet :: PackageSet ()
  }

-- | Default arguments of 'defaultMain'
--
-- Output file path is @sources.nix@.
defaultArgs :: Args
defaultArgs =
  Args
    ( \x ->
        x
          { shakeTimings = True,
            shakeProgress = progressSimple
          }
    )
    "sources.nix"
    (pure ())
    (pure ())
    (pure ())
    (pure ())

-- | Entry point of nvfetcher
runNvFetcher :: Args -> IO ()
runNvFetcher args = runNvFetcherWith [] $ const $ pure $ pure args

-- | Like 'runNvFetcher' but allows to define custom cli flags
runNvFetcherWith ::
  -- | Custom flags
  [OptDescr (Either String a)] ->
  -- | Continuation, the build system won't run if it returns Nothing
  ([a] -> IO (Maybe Args)) ->
  IO ()
runNvFetcherWith flags f = do
  var <- newMVar mempty
  shakeArgsOptionsWith
    shakeOptions
    flags
    $ \opts flagValues argValues -> runMaybeT $ do
      args@Args {..} <- MaybeT $ f flagValues
      pkgs <- liftIO $ Map.elems <$> runPackageSet argPackageSet
      let opts' =
            let old = argShakeOptions opts
             in old
                  { shakeExtra = addShakeExtra (VersionChanges var) (shakeExtra old),
                    -- rebuild everything if input packages have changed
                    -- this may leads to prefetch packages repeatedly
                    -- but input packages shouldn't be changed frequently
                    shakeVersion = "pkgs" <> show (hashWithSalt 0 pkgs) <> "-" <> shakeVersion old
                  }
          rules = mainRules args pkgs
      pure $ case argValues of
        [] -> (opts', want ["build"] >> rules)
        files -> (opts', want files >> rules)

mainRules :: Args -> [Package] -> Rules ()
mainRules Args {..} pkgs = do
  addHelpSuffix "It's important to keep .shake dir if you want to get correct version changes and proper cache"
  addHelpSuffix "Changing input packages will lead to a fully cleanup, requiring to rebuild everything next run"

  "clean" ~> do
    removeFilesAfter ".shake" ["//*"]
    removeFilesAfter "." [argOutputFilePath]
    argActionAfterClean

  "build" ~> do
    generateNixSources argOutputFilePath pkgs
    argActionAfterBuild

  argRules
  nvfetcherRules

--------------------------------------------------------------------------------

-- | Version change of a package
--
-- >>> VersionChange "foo" Nothing "2.3.3"
-- foo: ∅ → 2.3.3
--
-- >>> VersionChange "bar" (Just "2.2.2") "2.3.3"
-- bar: 2.2.2 → 2.3.3
data VersionChange = VersionChange
  { vcName :: PackageName,
    vcOld :: Maybe Version,
    vcNew :: Version
  }
  deriving (Eq)

instance Show VersionChange where
  show VersionChange {..} =
    T.unpack $ vcName <> ": " <> fromMaybe "∅" (coerce vcOld) <> " → " <> coerce vcNew

newtype VersionChanges = VersionChanges (MVar [VersionChange])

recordVersionChange :: PackageName -> Maybe Version -> Version -> Action ()
recordVersionChange vcName vcOld vcNew = do
  VersionChanges var <- fromJust <$> getShakeExtra @VersionChanges
  liftIO $ modifyMVar_ var (pure . (++ [VersionChange {..}]))

-- | Get version changes since the last run, relying on shake database.
--
-- Use this function in 'argActionAfterBuild' to produce external changelog
getVersionChanges :: Action [VersionChange]
getVersionChanges = do
  VersionChanges var <- fromJust <$> getShakeExtra @VersionChanges
  liftIO $ readMVar var

--------------------------------------------------------------------------------

-- | Rules of nvfetcher
nvfetcherRules :: Rules ()
nvfetcherRules = do
  nvcheckerRule
  prefetchRule

-- | Main action, given a set of packages, generating nix sources expr in a file
generateNixSources :: FilePath -> [Package] -> Action ()
generateNixSources fp pkgs = do
  body <- fmap genOne <$> actions
  getVersionChanges >>= \changes ->
    if null changes
      then putInfo "Up to date"
      else do
        putInfo "Changes:"
        putInfo $ unlines $ show <$> changes
  writeFileChanged fp $ T.unpack $ srouces $ T.unlines body
  putInfo $ "Generate " <> fp
  where
    single Package {..} = do
      (NvcheckerResult version mOld) <- checkVersion pversion
      prefetched <- prefetch $ pfetcher version
      case mOld of
        Nothing ->
          recordVersionChange pname Nothing version
        Just old
          | old /= version ->
            recordVersionChange pname (Just old) version
        _ -> pure ()
      pure (pname, version, prefetched)
    genOne (name, coerce @Version -> ver, toNixExpr -> srcP) =
      [trimming|
        $name = {
          pname = "$name";
          version = "$ver";
          src = $srcP;
        };
      |]
    actions = parallel $ map single pkgs
    srouces body =
      [trimming|
        # This file was generated by nvfetcher, please do not modify it manually.
        { fetchgit, fetchurl }:
        {
          $body
        }
      |]
