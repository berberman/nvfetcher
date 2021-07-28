{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Concurrent.STM
import Control.Exception (Handler (..), SomeException, bracket, catches, throwIO)
import Control.Monad (void)
import Data.Map (Map)
import Development.Shake
import Development.Shake.Database
import NvFetcher.Core (coreRules)
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import qualified System.IO.Extra as Extra
import System.Time.Extra
import Test.Hspec

--------------------------------------------------------------------------------

runAction :: ActionQueue -> Action a -> IO (Maybe a)
runAction chan x = do
  barrier <- newBarrier
  atomically $ writeTQueue chan $ x >>= liftIO . signalBarrier barrier
  -- TODO
  timeout 30 $ waitBarrier barrier

type ActionQueue = TQueue (Action ())

newAsyncActionQueue :: Map PackageKey Package -> IO (ActionQueue, Async ())
newAsyncActionQueue pkgs = Extra.withTempDir $ \dir -> do
  shakeExtras <- liftIO $ initShakeExtras pkgs 3
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

--------------------------------------------------------------------------------

aroundShake :: SpecWith ActionQueue -> Spec
aroundShake = aroundShake' mempty

aroundShake' :: Map PackageKey Package -> SpecWith ActionQueue -> Spec
aroundShake' pkgs = aroundAll $ \f ->
  bracket
    (newAsyncActionQueue pkgs)
    (\(_, runnerTask) -> cancel runnerTask)
    (\(chan, _) -> f chan)

shouldReturnJust :: (Show a, Eq a) => IO (Maybe a) -> a -> Expectation
shouldReturnJust f x = f `shouldReturn` Just x
