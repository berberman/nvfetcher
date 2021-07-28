{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchRustGitDepsSpec where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import NvFetcher.FetchRustGitDeps
import NvFetcher.NixFetcher
import NvFetcher.Types
import Test.Hspec
import Utils

spec :: Spec
spec = aroundShake $
  describe "fetchRustGitDeps" $
    it "works" $ \chan -> do
      prefetched <-
        runAction chan $
          prefetch $
            gitFetcher
              "https://gist.github.com/NickCao/6c4dbc4e15db5da107de6cdb89578375"
              "8a5f37a8f80a3b05290707febf57e88661cee442"
      prefetched `shouldSatisfy` isJust
      runFetchRustGitDepsRule chan (fromJust prefetched) "Cargo.lock"
        `shouldReturnJust` HMap.fromList
          [ ("rand-0.8.3", Checksum "1khg0rnz8xxd389cprqmy9vq9sggzz78lb9n7hh2w6xfsl4xfyyc")
          ]

runFetchRustGitDepsRule :: ActionQueue -> NixFetcher Fetched -> FilePath -> IO (Maybe (HashMap Text Checksum))
runFetchRustGitDepsRule chan fetcher lockPath = runAction chan $ fetchRustGitDeps fetcher lockPath
