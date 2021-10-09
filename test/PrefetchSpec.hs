{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PrefetchSpec where

import Control.Monad.Trans.Reader
import NvFetcher.NixFetcher
import NvFetcher.Types
import Test.Hspec
import Utils

spec :: Spec
spec = aroundShake $
  describe "fetchers" $ do
    specifyChan "pypi" $
      runPrefetchRule (pypiFetcher "example" "0.1.0")
        `shouldReturnJust` Checksum "sha256-9Yc+bshBd2SXwNQqUVP4TC8S+mjqTXe+FGfouan/w7s="

    specifyChan "openvsx" $
      runPrefetchRule (openVsxFetcher ("usernamehw", "indent-one-space") "0.2.6")
        `shouldReturnJust` Checksum "sha256-oS2ERs/uEDJx5J/N67STLaUf/hY2RyErjipAEH45q2o="

    specifyChan "vsmarketplace" $
      runPrefetchRule (vscodeMarketplaceFetcher ("usernamehw", "indent-one-space") "0.2.6")
        `shouldReturnJust` Checksum "sha256-h6dBBlsnl6Q7vHUjrnezmjn3EsZHF+Q35BLt1SARuO4="

    specifyChan "git" $
      runPrefetchRule (gitFetcher "https://gitlab.com/gitlab-org/gitlab-test.git" "ddd0f15ae83993f5cb66a927a28673882e99100b")
        `shouldReturnJust` Checksum "sha256-LHRlNq6YKUXAKjr2m+BOmzK7veLadIUk998ycxahVn4="

    specifyChan "github" $
      runPrefetchRule (gitHubFetcher ("harry-sanabria", "ReleaseTestRepo") "release3")
        `shouldReturnJust` Checksum "sha256-cSygC4nBg8ChArw+eGSS0PBE5n6Tc0nJLdxEmaDYGKk="

--------------------------------------------------------------------------------

runPrefetchRule :: NixFetcher Fresh -> ReaderT ActionQueue IO (Maybe Checksum)
runPrefetchRule f = runActionChan $ _sha256 <$> prefetch f
