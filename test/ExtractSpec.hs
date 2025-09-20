{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ExtractSpec where

import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Development.Shake (liftIO)
import Development.Shake.FilePath ((</>))
import FetchRustGitDepsSpec (runPrefetchRule)
import NvFetcher (getBuildDir)
import NvFetcher.ExtractSrc
import NvFetcher.NixFetcher
import NvFetcher.Types
import qualified System.Directory.Extra as IO
import Test.Hspec
import Utils

spec :: Spec
spec = aroundShake $ do
  describe "extract" $ do
    let fetcher = gitHubFetcher ("berberman", "nvfetcher") $ Version "0.7.0"
        srcSha256 = "sha256-GVjVivAz2AW7WNqpWTA5Uy9vN8Wh_3pjuZRFhsGVk24="
        mkExpectedMap files = HMap.fromList [(f, srcSha256 </> f) | f <- files]
        mkExpectedPaths buildDir files = [buildDir </> srcSha256 </> f | f <- files]
        allExist = mapM_ shouldExist
    specifyChan "extracts single file" $ do
      prefetched <- runPrefetchRule fetcher
      shouldBeJust prefetched
      let expectedFiles = ["Setup.hs"]
      runExtractSrcsRule (fromJust prefetched) (NE.singleton "Setup.hs")
        `shouldReturnJust` mkExpectedMap expectedFiles
      (fromJust -> buildDir) <- runActionChan getBuildDir
      allExist $ mkExpectedPaths buildDir expectedFiles
    specifyChan "extracts files" $ do
      prefetched <- runPrefetchRule fetcher
      shouldBeJust prefetched
      let expectedFiles =
            [ "default.nix",
              "CHANGELOG.md",
              "README.md",
              "flake.nix",
              "nix/default.nix"
            ]
      runExtractSrcsRule (fromJust prefetched) (NE.fromList ["**/*.nix", "*.md"])
        `shouldReturnJust` mkExpectedMap expectedFiles
      (fromJust -> buildDir) <- runActionChan getBuildDir
      allExist $ mkExpectedPaths buildDir expectedFiles
    specifyChan "extracts rerun" $ do
      prefetched <- runPrefetchRule fetcher
      shouldBeJust prefetched
      let expectedFiles = ["Setup.hs"]
      runExtractSrcsRule (fromJust prefetched) (NE.singleton "Setup.hs")
        `shouldReturnJust` mkExpectedMap expectedFiles
      (fromJust -> buildDir) <- runActionChan getBuildDir
      let path = case mkExpectedPaths buildDir expectedFiles of
            [] -> error "Impossible"
            (p : _) -> p
      allExist [path]
      -- Delete this generated file and rerun the rule
      -- We should see the file being regenerated
      liftIO $ IO.removeFile path
      -- Run again
      runExtractSrcsRule (fromJust prefetched) (NE.singleton "Setup.hs")
        `shouldReturnJust` mkExpectedMap expectedFiles
      -- Check that the file has been regenerated
      allExist [path]

runExtractSrcsRule :: NixFetcher Fetched -> NE.NonEmpty Glob -> ReaderT ActionQueue IO (Maybe (HMap.HashMap FilePath FilePath))
runExtractSrcsRule fetcher globs = runActionChan $ extractSrcs fetcher globs
