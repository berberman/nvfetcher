{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Concurrent.Extra
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forever, join)
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Development.Shake
import NvFetcher.Core (coreRules)
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import qualified System.IO.Extra as Extra
import Test.Hspec

fakePackageKey :: PackageKey
fakePackageKey = PackageKey "a-fake-package"

fakePackage :: Package
fakePackage =
  Package
    { _pname = coerce fakePackageKey,
      _pversion = undefined,
      _pfetcher = undefined,
      _pcargo = undefined,
      _pextract = undefined,
      _ppassthru = undefined
    }

--------------------------------------------------------------------------------

-- | We need a fakePackageKey here; otherwise the nvchecker rule would be cutoff
runAction :: ActionQueue -> Action a -> IO a
runAction chan x = do
  barrier <- newBarrier
  atomically $ writeTQueue chan $ x >>= liftIO . signalBarrier barrier
  waitBarrier barrier

type ActionQueue = TQueue (Action ())

newAsyncActionQueue :: IO (ActionQueue, ThreadId)
newAsyncActionQueue = Extra.withTempDir $ \dir -> do
  shakeExtras <- liftIO $ initShakeExtras (Map.singleton fakePackageKey fakePackage) 3
  chan <- atomically newTQueue
  thread <- forkIO $
    shake
      shakeOptions
        { shakeExtra = addShakeExtra shakeExtras (shakeExtra shakeOptions),
          shakeFiles = dir,
          shakeVerbosity = Quiet
        }
      $ do
        coreRules
        action $ forever $ join $ liftIO $ atomically $ readTQueue chan
  pure (chan, thread)

aroundActionQueue :: SpecWith ActionQueue -> Spec
aroundActionQueue = around $ \f ->
  bracket
    newAsyncActionQueue
    (\(_, thread) -> killThread thread)
    (\(chan, _) -> f chan)

--------------------------------------------------------------------------------

enqueueNvchecker :: ActionQueue -> VersionSource -> IO Version
enqueueNvchecker chan v = runAction chan $ nvNow <$> checkVersion v def fakePackageKey

enqueuePrefetch :: ActionQueue -> NixFetcher Fresh -> IO Checksum
enqueuePrefetch chan f = runAction chan $ _sha256 <$> prefetch f
