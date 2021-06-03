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

    -- * Retries
    withRetries,
  )
where

import Control.Concurrent.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Development.Shake
import NvFetcher.Types

-- | Values we use during the build. It's stored in 'shakeExtra'
data ShakeExtras = ShakeExtras
  { versionChanges :: Var [VersionChange],
    targetPackages :: Map PackageKey Package,
    retries :: Int
  }

-- | Get our values from shake
getShakeExtras :: Action ShakeExtras
getShakeExtras =
  getShakeExtra @ShakeExtras >>= \case
    Just x -> pure x
    _ -> fail "ShakeExtras is missing!"

-- | Create an empty 'ShakeExtras' from packages to build and times to retry for each rule
initShakeExtras :: Map PackageKey Package -> Int -> IO ShakeExtras
initShakeExtras targetPackages retries = do
  versionChanges <- newVar mempty
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

-- | Run an action, retry at most 'retries' times if it throws an exception
withRetries :: Action a -> Action a
withRetries a = getShakeExtras >>= \ShakeExtras {..} -> actionRetry retries a
