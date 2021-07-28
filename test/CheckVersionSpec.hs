{-# LANGUAGE OverloadedStrings #-}

module CheckVersionSpec where

import Data.Default (def)
import NvFetcher.Nvchecker
import NvFetcher.Types
import Test.Hspec
import Utils

spec :: Spec
spec = aroundShake $
  describe "nvchecker" $ do
    specify "pypi" $ \chan ->
      runNvcheckerRule chan (Pypi "example") `shouldReturnJust` Version "0.1.0"

    specify "archpkg" $ \chan ->
      runNvcheckerRule chan (ArchLinux "ipw2100-fw") `shouldReturnJust` Version "1.3"

    specify "aur" $ \chan ->
      runNvcheckerRule chan (Aur "ssed") `shouldReturnJust` Version "3.62"

    specify "git" $ \chan ->
      runNvcheckerRule chan (Git "https://gitlab.com/gitlab-org/gitlab-test.git" def)
        `shouldReturnJust` Version "ddd0f15ae83993f5cb66a927a28673882e99100b"

    specify "github latest release" $ \chan ->
      runNvcheckerRule chan (GitHubRelease "harry-sanabria" "ReleaseTestRepo")
        `shouldReturnJust` Version "release3"

    specify "github max tag" $ \chan ->
      runNvcheckerRule chan (GitHubTag "harry-sanabria" "ReleaseTestRepo" def)
        `shouldReturnJust` "second_release"

    specify "github max tag with ignored" $ \chan ->
      runNvcheckerRule chan (GitHubTag "harry-sanabria" "ReleaseTestRepo" def {_ignored = Just "second_release release3"})
        `shouldReturnJust` Version "first_release"

    specify "http header" $ \chan ->
      shouldBeJust $ runNvcheckerRule chan (HttpHeader "https://www.unifiedremote.com/download/linux-x64-deb" "urserver-([\\d.]+).deb" def)

    specify "manual" $ \chan ->
      runNvcheckerRule chan (Manual "Meow") `shouldReturnJust` Version "Meow"

    specify "openvsx" $ \chan ->
      runNvcheckerRule chan (OpenVsx "usernamehw" "indent-one-space") `shouldReturnJust` Version "0.2.6"

    specify "repology" $ \chan ->
      runNvcheckerRule chan (Repology "ssed" "aur") `shouldReturnJust` Version "3.62"

    specify "vsmarketplace" $ \chan ->
      runNvcheckerRule chan (VscodeMarketplace "usernamehw" "indent-one-space") `shouldReturnJust` Version "0.2.6"
