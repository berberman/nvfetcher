{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Concurrent.STM
import Control.Exception (Handler (..), SomeException, bracket, catches, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Map (Map)
import Data.Maybe (isJust)
import Development.Shake
import Development.Shake.Database
import NvFetcher.Core (coreRules)
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras
import qualified System.IO.Extra as Extra
import System.Time.Extra
import Test.Hspec
import UnliftIO (MonadUnliftIO (withRunInIO))

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
  shakeExtras <- liftIO $ initShakeExtras pkgs 3 dir mempty
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

  runnerTask <- asyncWithUnmask $ \r -> runner r
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

shouldReturnJust :: (Show a, Eq a, MonadUnliftIO m) => m (Maybe a) -> a -> m ()
shouldReturnJust f x = withRunInIO $ \run -> run f `shouldReturn` Just x

shouldBeJust :: (MonadIO m, Show a) => Maybe a -> m ()
shouldBeJust x = liftIO $ x `shouldSatisfy` isJust

specifyChan :: HasCallStack => String -> ReaderT ActionQueue IO () -> SpecWith ActionQueue
specifyChan s m = specify s $ \r -> runReaderT m r

runActionChan :: Action a -> ReaderT ActionQueue IO (Maybe a)
runActionChan m = ask >>= \chan -> liftIO $ runAction chan m
