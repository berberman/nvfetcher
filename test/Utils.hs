{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Concurrent.STM
import Control.Exception (Handler (..), SomeException, bracket, catches, throwIO)
import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Development.Shake
import Development.Shake.Database
import NvFetcher.Core (coreRules)
import NvFetcher.NixFetcher
import NvFetcher.Nvchecker
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import qualified System.IO.Extra as Extra
import System.Time.Extra
import Test.Hspec

-- | We need a fakePackageKey here; otherwise the nvchecker rule would be cutoff
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

runAction :: ActionQueue -> Action a -> IO (Maybe a)
runAction chan x = do
  barrier <- newBarrier
  atomically $ writeTQueue chan $ x >>= liftIO . signalBarrier barrier
  -- TODO
  timeout 10 $ waitBarrier barrier

type ActionQueue = TQueue (Action ())

newAsyncActionQueue :: IO (ActionQueue, Async ())
newAsyncActionQueue = Extra.withTempDir $ \dir -> do
  shakeExtras <- liftIO $ initShakeExtras (Map.singleton fakePackageKey fakePackage) 3
  chan <- atomically newTQueue
  (getShakeDb, _) <-
    shakeOpenDatabase
      shakeOptions
        { shakeExtra = addShakeExtra shakeExtras (shakeExtra shakeOptions),
          shakeFiles = dir,
          shakeVerbosity = Quiet
        }
      coreRules
  shakeDb <- getShakeDb

  let runner restore = do
        -- sequentially
        act <- liftIO $ atomically $ readTQueue chan
        catches
          (restore $ void $ shakeRunDatabase shakeDb [act])
          [ Handler $ \(e :: AsyncCancelled) -> throwIO e,
            Handler $ \(e :: SomeException) -> putStrLn $ "an exception arose in action runner: " <> show e
          ]
        runner restore

  runnerTask <- asyncWithUnmask runner
  pure (chan, runnerTask)

aroundShake :: SpecWith ActionQueue -> Spec
aroundShake = aroundAll $ \f ->
  bracket
    newAsyncActionQueue
    (\(_, runnerTask) -> cancel runnerTask)
    (\(chan, _) -> f chan)

--------------------------------------------------------------------------------

runNvcheckerRule :: ActionQueue -> VersionSource -> IO (Maybe Version)
runNvcheckerRule chan v = runAction chan $ nvNow <$> checkVersion v def fakePackageKey

runPrefetchRule :: ActionQueue -> NixFetcher Fresh -> IO (Maybe Checksum)
runPrefetchRule chan f = runAction chan $ _sha256 <$> prefetch f

shouldReturnJust :: (Show a, Eq a) => IO (Maybe a) -> a -> Expectation
shouldReturnJust f x = f `shouldReturn` Just x

shouldBeJust :: Show a => IO (Maybe a) -> Expectation
shouldBeJust f = f >>= \x -> shouldSatisfy x isJust
