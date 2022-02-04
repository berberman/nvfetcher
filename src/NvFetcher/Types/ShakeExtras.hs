{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module is about global information we use in rules.
module NvFetcher.Types.ShakeExtras
  ( -- * Types
    ShakeExtras (..),
    initShakeExtras,
    getShakeExtras,

    -- * Packages
    lookupPackage,
    getAllPackageKeys,
    isPackageKeyTarget,

    -- * Version changes
    recordVersionChange,
    getVersionChanges,

    -- * Retry
    withRetry,

    -- * Build dir
    getBuildDir,

    -- * Last versions
    getLastVersionOnDisk,
    getRecentLastVersion,
    updateLastVersion,
    getAllOnDiskVersions,
  )
where

import Control.Concurrent.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Development.Shake
import NvFetcher.Config
import NvFetcher.Types

data LastVersion
  = OnDisk Version
  | Updated
      (Maybe Version)
      -- ^ on disk if has
      Version

-- | Values we use during the build. It's stored in 'shakeExtra'
data ShakeExtras = ShakeExtras
  { config :: Config,
    versionChanges :: Var [VersionChange],
    targetPackages :: Map PackageKey Package,
    lastVersions :: Var (Map PackageKey LastVersion)
  }

-- | Get our values from shake
getShakeExtras :: Action ShakeExtras
getShakeExtras =
  getShakeExtra @ShakeExtras >>= \case
    Just x -> pure x
    _ -> fail "ShakeExtras is missing!"

-- | Create an empty 'ShakeExtras' from packages to build, times to retry for each rule,
-- build dir, and on disk versions
initShakeExtras :: Config -> Map PackageKey Package -> Map PackageKey Version -> IO ShakeExtras
initShakeExtras config targetPackages lv = do
  versionChanges <- newVar mempty
  lastVersions <- newVar $ Map.map OnDisk lv
  pure ShakeExtras {..}

-- | Get keys of all packages to build
getAllPackageKeys :: Action [PackageKey]
getAllPackageKeys = do
  ShakeExtras {..} <- getShakeExtras
  pure $ Map.keys targetPackages

-- | Find a package given its key
lookupPackage :: PackageKey -> Action (Maybe Package)
lookupPackage key = do
  ShakeExtras {..} <- getShakeExtras
  pure $ Map.lookup key targetPackages

-- | Check if we need build this package
isPackageKeyTarget :: PackageKey -> Action Bool
isPackageKeyTarget k = Map.member k . targetPackages <$> getShakeExtras

-- | Record version change of a package
recordVersionChange :: PackageName -> Maybe Version -> Version -> Action ()
recordVersionChange vcName vcOld vcNew = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ versionChanges (pure . (++ [VersionChange {..}]))

-- | Get version changes since the last run
getVersionChanges :: Action [VersionChange]
getVersionChanges = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ readVar versionChanges

-- | Run an action, retry at most 'retry' times (defined in config) if it throws an exception
withRetry :: Action a -> Action a
withRetry a = getShakeExtras >>= \ShakeExtras {..} -> actionRetry (retry config) a

-- | Get build dir
getBuildDir :: Action FilePath
getBuildDir = buildDir . config <$> getShakeExtras

-- | Get initial version of a package
getLastVersionOnDisk :: PackageKey -> Action (Maybe Version)
getLastVersionOnDisk k = do
  ShakeExtras {..} <- getShakeExtras
  versions <- liftIO $ readVar lastVersions
  pure $ case versions Map.!? k of
    Just (Updated v _) -> v
    Just (OnDisk v) -> Just v
    _ -> Nothing

-- | Get version of a package, no matter it was initial one or rule result
getRecentLastVersion :: PackageKey -> Action (Maybe Version)
getRecentLastVersion k = do
  ShakeExtras {..} <- getShakeExtras
  versions <- liftIO $ readVar lastVersions
  pure $ case versions Map.!? k of
    Just (Updated _ v) -> Just v
    Just (OnDisk v) -> Just v
    _ -> Nothing

-- | Add nvchecker result of a package
updateLastVersion :: PackageKey -> Version -> Action ()
updateLastVersion k v = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $
    modifyVar_ lastVersions $ \versions -> pure $ case versions Map.!? k of
      Just (Updated o _) -> Map.insert k (Updated o v) versions
      Just (OnDisk lv) -> Map.insert k (Updated (Just lv) v) versions
      _ -> Map.insert k (Updated Nothing v) versions

-- | Get all initial versions
getAllOnDiskVersions :: Action (Map PackageKey Version)
getAllOnDiskVersions = do
  ShakeExtras {..} <- getShakeExtras
  versions <- liftIO $ readVar lastVersions
  let xs = Map.toList $
        flip Map.map versions $ \case
          OnDisk v -> Just v
          Updated v _ -> v
  pure $ Map.fromList [(k, v) | (k, Just v) <- xs]
