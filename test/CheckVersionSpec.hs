{-# LANGUAGE OverloadedStrings #-}

module CheckVersionSpec where

import Control.Monad.Trans.Reader
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Lens.Micro
import NvFetcher.Nvchecker
import NvFetcher.Types
import NvFetcher.Types.Lens
import Test.Hspec
import Utils

-- | We need a fakePackageKey here; otherwise the nvchecker rule would be cutoff
spec :: Spec
spec = aroundShake' (Map.singleton fakePackageKey fakePackage) $
  describe "nvchecker" $ do
    specifyChan "pypi" $
      runNvcheckerRule (Pypi "example") `shouldReturnJust` Version "0.1.0"

    specifyChan "archpkg" $
      runNvcheckerRule (ArchLinux "ipw2100-fw") `shouldReturnJust` Version "1.3"

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

    specifyChan "http header" $ do
      runNvcheckerRule (HttpHeader "https://www.unifiedremote.com/download/linux-x64-deb" "urserver-([\\d.]+).deb" def)
        >>= shouldBeJust

    specifyChan "manual" $
      runNvcheckerRule (Manual "Meow") `shouldReturnJust` Version "Meow"

    specifyChan "openvsx" $
      runNvcheckerRule (OpenVsx "usernamehw" "indent-one-space") `shouldReturnJust` Version "0.2.6"

    specifyChan "repology" $
      runNvcheckerRule (Repology "ssed" "aur") `shouldReturnJust` Version "3.62"

    specifyChan "vsmarketplace" $
      runNvcheckerRule (VscodeMarketplace "usernamehw" "indent-one-space") `shouldReturnJust` Version "0.2.6"

--------------------------------------------------------------------------------

runNvcheckerRule :: VersionSource -> ReaderT ActionQueue IO (Maybe Version)
runNvcheckerRule v = runActionChan $ nvNow <$> checkVersion v def fakePackageKey

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
