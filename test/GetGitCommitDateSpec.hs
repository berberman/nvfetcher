{-# LANGUAGE OverloadedStrings #-}

module GetGitCommitDateSpec where

import Control.Monad.Trans.Reader
import Data.Default (def)
import Data.Text (Text)
import NvFetcher.GetGitCommitDate
import Test.Hspec
import Utils

spec :: Spec
spec =
  aroundShake $
    describe "get git commit date" $ do
      specifyChan "default format" $
        runGetGitCommitDateRule repo rev def
          `shouldReturnJust` "2021-05-31"
      specifyChan "custom format" $
        runGetGitCommitDateRule repo rev (DateFormat $ Just "%Y-%m-%d %H:%M:%S")
          `shouldReturnJust` "2021-05-31 18:43:48"

repo :: Text
repo = "https://gist.github.com/NickCao/6c4dbc4e15db5da107de6cdb89578375"

rev :: Text
rev = "8a5f37a8f80a3b05290707febf57e88661cee442"

runGetGitCommitDateRule :: Text -> Text -> DateFormat -> ReaderT ActionQueue IO (Maybe Text)
runGetGitCommitDateRule url rev format = runActionChan $ getGitCommitDate url rev format
