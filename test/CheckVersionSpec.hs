{-# LANGUAGE OverloadedStrings #-}

module CheckVersionSpec where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Lens.Micro
import NvFetcher.Config (Config (cacheNvchecker))
import NvFetcher.Nvchecker
import NvFetcher.Types
import NvFetcher.Types.Lens
import System.IO.Extra (newTempFile)
import Test.Hspec
import Utils

spec :: Spec
spec = do
  versionSourcesSpec
  useStaleSpec
  cacheSpec

versionSourcesSpec :: Spec
versionSourcesSpec = aroundShake $
  describe "nvchecker" $ do
    specifyChan "pypi" $
      runNvcheckerRule (Pypi "example") `shouldReturnJust` Version "0.1.0"

    -- specifyChan "archpkg" $
    --   runNvcheckerRule (ArchLinux "ipw2100-fw") `shouldReturnJust` Version "1.3"

    specifyChan "aur" $
      runNvcheckerRule (Aur "ssed") `shouldReturnJust` Version "3.62"

    specifyChan "git" $
      runNvcheckerRule
        (Git "https://gitlab.com/gitlab-org/gitlab-test.git" def)
        `shouldReturnJust` Version "ddd0f15ae83993f5cb66a927a28673882e99100b"

    specifyChan "github latest release" $
      runNvcheckerRule (GitHubRelease "harry-sanabria" "ReleaseTestRepo")
        `shouldReturnJust` Version "release3"

    specifyChan "github max tag" $
      runNvcheckerRule (GitHubTag "harry-sanabria" "ReleaseTestRepo" def)
        `shouldReturnJust` "second_release"

    specifyChan "github max tag with ignored" $
      runNvcheckerRule (GitHubTag "harry-sanabria" "ReleaseTestRepo" $ def & ignored ?~ "second_release release3")
        `shouldReturnJust` Version "first_release"

    specifyChan "http header" $
      runNvcheckerRule (HttpHeader "https://www.unifiedremote.com/download/linux-x64-deb" "urserver-([\\d.]+).deb" def)
        >>= shouldBeJust

    -- specifyChan "webpage" $
    --   runNvcheckerRule (Webpage "http://ftp.vim.org/pub/vim/patches/7.3/" "7\\.3\\.\\d+" def)
    --     `shouldReturnJust` Version "7.3.1314"

    specifyChan "manual" $
      runNvcheckerRule (Manual "Meow") `shouldReturnJust` Version "Meow"

    specifyChan "openvsx" $
      runNvcheckerRule (OpenVsx "usernamehw" "indent-one-space") `shouldReturnJust` Version "0.2.7"

    specifyChan "repology" $
      runNvcheckerRule (Repology "ssed" "aur") `shouldReturnJust` Version "3.62"

    specifyChan "vsmarketplace" $
      runNvcheckerRule (VscodeMarketplace "usernamehw" "indent-one-space") `shouldReturnJust` Version "0.2.8"

    specifyChan "cmd" $
      runNvcheckerRule (Cmd "echo Meow") `shouldReturnJust` Version "Meow"

--------------------------------------------------------------------------------

-- | We need disable nvchecker cache to see if use stale works
useStaleSpec :: Spec
useStaleSpec = aroundShake' (Map.singleton fakePackageKey fakePinnedPackage) def {cacheNvchecker = False} $
  describe "useStale" $ do
    (temp, cleanup) <- runIO newTempFile

    let versionSource = Cmd $ "cat " <> T.pack temp

    specifyChan "needs run" $ do
      liftIO $ writeFile temp "Meow"
      runNvcheckerRuleOnFakePackage versionSource `shouldReturnJust` NvcheckerResult {nvNow = "Meow", nvOld = Nothing, nvStale = False}

    specifyChan "stale" $ do
      liftIO $ writeFile temp "Bark"
      runNvcheckerRuleOnFakePackage versionSource `shouldReturnJust` NvcheckerResult {nvNow = "Meow", nvOld = Just "Meow", nvStale = True}

    runIO cleanup

cacheSpec :: Spec
cacheSpec = aroundShake' (Map.singleton fakePackageKey fakePackage) def {cacheNvchecker = True} $
  describe "cache" $ do
    (temp, cleanup) <- runIO newTempFile

    let versionSource = Cmd $ "cat " <> T.pack temp

    specifyChan "needs run" $ do
      liftIO $ writeFile temp "Meow"
      runNvcheckerRuleOnFakePackage versionSource `shouldReturnJust` NvcheckerResult {nvNow = "Meow", nvOld = Nothing, nvStale = False}

    specifyChan "cache" $ do
      liftIO $ writeFile temp "Bark"
      runNvcheckerRuleOnFakePackage versionSource `shouldReturnJust` NvcheckerResult {nvNow = "Meow", nvOld = Just "Meow", nvStale = False}

    runIO cleanup

--------------------------------------------------------------------------------

runNvcheckerRule :: VersionSource -> ReaderT ActionQueue IO (Maybe Version)
runNvcheckerRule v = fmap nvNow <$> runActionChan (checkVersion' v def)

runNvcheckerRuleOnFakePackage :: VersionSource -> ReaderT ActionQueue IO (Maybe NvcheckerResult)
runNvcheckerRuleOnFakePackage v = runActionChan $ checkVersion v def fakePackageKey

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
      _ppassthru = undefined,
      _ppinned = NoStale
    }

fakePinnedPackage :: Package
fakePinnedPackage = fakePackage {_ppinned = PermanentStale}
