{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchRustGitDepsSpec where

import Control.Monad.Trans.Reader
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (fromJust)
import Data.Text (Text)
import NvFetcher.FetchRustGitDeps
import NvFetcher.NixFetcher
import NvFetcher.Types
import Test.Hspec
import Utils

spec :: Spec
spec = aroundShake $
  describe "fetchRustGitDeps" $
    specifyChan "works" $ do
      prefetched <-
        runPrefetchRule $
          gitFetcher
            "https://gist.github.com/NickCao/6c4dbc4e15db5da107de6cdb89578375"
            "8a5f37a8f80a3b05290707febf57e88661cee442"
      shouldBeJust prefetched
      runFetchRustGitDepsRule (fromJust prefetched) "Cargo.lock"
        `shouldReturnJust` HMap.fromList
          [ ("rand-0.8.3", Checksum "sha256-zHvXCdWuGy4gPDYtis7/7+mEd/IV58sSGq139G0GD84=")
          ]

runPrefetchRule :: NixFetcher Fresh -> ReaderT ActionQueue IO (Maybe (NixFetcher Fetched))
runPrefetchRule fetcher = runActionChan $ prefetch fetcher

runFetchRustGitDepsRule :: NixFetcher Fetched -> FilePath -> ReaderT ActionQueue IO (Maybe (HashMap Text Checksum))
runFetchRustGitDepsRule fetcher lockPath = runActionChan $ fetchRustGitDeps fetcher lockPath
