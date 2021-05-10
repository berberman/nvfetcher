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
module NvFetcher.ShakeExtras
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
  )
where

import Control.Concurrent.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Development.Shake
import NvFetcher.Types

-- | Values we use during the build. It's stored in 'shakeExtra'
data ShakeExtras = ShakeExtras
  { versionChanges :: Var [VersionChange],
    targetPackages :: HashMap PackageKey Package
  }

-- | Get our values from shake
getShakeExtras :: Action ShakeExtras
getShakeExtras =
  getShakeExtra @ShakeExtras >>= \case
    Just x -> pure x
    _ -> fail "ShakeExtras is missing!"

-- | Create an empty 'ShakeExtras' from packages to build
initShakeExtras :: HashMap PackageKey Package -> IO ShakeExtras
initShakeExtras targetPackages = do
  versionChanges <- newVar mempty
  pure ShakeExtras {..}

-- | Get keys of all packages to build
getAllPackageKeys :: Action [PackageKey]
getAllPackageKeys = do
  ShakeExtras {..} <- getShakeExtras
  pure $ HMap.keys targetPackages

-- | Find a package given its key
lookupPackage :: PackageKey -> Action (Maybe Package)
lookupPackage key = do
  ShakeExtras {..} <- getShakeExtras
  pure $ HMap.lookup key targetPackages

-- | Check if we need build this package
isPackageKeyTarget :: PackageKey -> Action Bool
isPackageKeyTarget k = HMap.member k . targetPackages <$> getShakeExtras

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
