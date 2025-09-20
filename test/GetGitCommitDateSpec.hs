{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GetGitCommitDateSpec where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTimeZone, parseTimeM, utcToLocalTime)
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
        runGetGitCommitDateRule repo rev (GitDateFormat $ Just "%Y-%m-%d %H:%M:%S", def)
          `shouldReturnJust` "2021-05-31 18:43:48"
      specifyChan "UTC time zone" $
        runGetGitCommitDateRule repo rev (GitDateFormat $ Just "%H:%M:%S", GitTimeZone $ Just "UTC")
          -- The commit was made at UTC+8; hence we expect the time to be 8 hours earlier
          `shouldReturnJust` "10:43:48"
      specifyChan "local time zone" $ do
        -- Get the current time zone
        tz <- liftIO getCurrentTimeZone
        -- Get commit time at UTC
        rawCommitTime <- runGetGitCommitDateRule repo rev (GitDateFormat $ Just "%Y-%m-%d %H:%M:%S", GitTimeZone $ Just "UTC")
        shouldBeJust rawCommitTime
        parsedCommitTime :: UTCTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . T.unpack . fromJust $ rawCommitTime
        -- Convert UTC time to local time
        let localTimeStr = T.pack $ formatTime defaultTimeLocale "%H:%M:%S" $ utcToLocalTime tz parsedCommitTime
        -- Rule should return the same result as we computed
        runGetGitCommitDateRule repo rev (GitDateFormat $ Just "%H:%M:%S", GitTimeZone $ Just "local")
          `shouldReturnJust` localTimeStr

repo :: Text
repo = "https://gist.github.com/NickCao/6c4dbc4e15db5da107de6cdb89578375"

rev :: Text
rev = "8a5f37a8f80a3b05290707febf57e88661cee442"

runGetGitCommitDateRule :: Text -> Text -> (GitDateFormat, GitTimeZone) -> ReaderT ActionQueue IO (Maybe Text)
runGetGitCommitDateRule url rev (format, tz) = runActionChan $ getGitCommitDate url rev (format, tz)
