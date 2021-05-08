{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module NvFetcher.ShakeExtras where

import Control.Concurrent.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Development.Shake
import NvFetcher.Types

data ShakeExtras = ShakeExtras
  { versionChanges :: Var [VersionChange],
    targetPackages :: HashMap PackageKey Package
  }

getShakeExtras :: Action ShakeExtras
getShakeExtras =
  getShakeExtra @ShakeExtras >>= \case
    Just x -> pure x
    _ -> fail "ShakeExtras is missing!"

initShakeExtras :: HashMap PackageKey Package -> IO ShakeExtras
initShakeExtras targetPackages = do
  versionChanges <- newVar mempty
  pure ShakeExtras {..}

getAllPackageKeys :: Action [PackageKey]
getAllPackageKeys = do
  ShakeExtras {..} <- getShakeExtras
  pure $ HMap.keys targetPackages

getPackageByKey :: PackageKey -> Action (Maybe Package)
getPackageByKey key = do
  ShakeExtras {..} <- getShakeExtras
  pure $ HMap.lookup key targetPackages

isPackageKeyTarget :: PackageKey -> Action Bool
isPackageKeyTarget k = HMap.member k . targetPackages <$> getShakeExtras

-- | Record version change of a package
recordVersionChange :: PackageName -> Maybe Version -> Version -> Action ()
recordVersionChange vcName vcOld vcNew = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ versionChanges (pure . (++ [VersionChange {..}]))

-- | Get version changes since the last run, relying on shake database
getVersionChanges :: Action [VersionChange]
getVersionChanges = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ readVar versionChanges
