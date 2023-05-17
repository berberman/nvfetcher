{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PrefetchSpec where

import Control.Arrow ((&&&))
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
      runPrefetchRule (gitFetcher "https://github.com/git-up/test-repo-submodules" "5a1dfa807759c39e3df891b6b46dfb2cf776c6ef")
        `shouldReturnJust` Checksum "sha256-wCo1YobyatxSOE85xQNSJw6jvufghFNHlZl4ToQjRHA="

    specifyChan "github" $
      runPrefetchRule (gitHubFetcher ("harry-sanabria", "ReleaseTestRepo") "release3")
        `shouldReturnJust` Checksum "sha256-cSygC4nBg8ChArw+eGSS0PBE5n6Tc0nJLdxEmaDYGKk="

    specifyChan "tarball" $
      runPrefetchRule (tarballFetcher "https://github.com/nixos/nixpkgs/archive/3d35529a48d3ad50ad959463755b0b7fe392cfa7.tar.gz")
        `shouldReturnJust` Checksum "sha256-TwfXEION3DcOivzDqXSKNf1PNTZWF124nOF/UbZGRlE="

    specifyChan "docker" $
      runPrefetchRule' (_sha256 &&& _imageDigest) testDockerFetcher
        `shouldReturnJust`
          ( Checksum "sha256-uaJxeiRm94tWDBTe51/KwUBKR2vj9i4i3rhotsYPxtM=",
            ContainerDigest "sha256:65a2763f593ae85fab3b5406dc9e80f744ec5b449f269b699b5efd37a07ad32e"
          )

testDockerFetcher :: NixFetcher Fresh
testDockerFetcher =
  FetchDocker
    { _imageName = "library/alpine",
      _imageTag = "3.16.2",
      _imageDigest = (),
      _sha256 = (),
      _fos = Nothing,
      _farch = Nothing,
      _finalImageName = Nothing,
      _finalImageTag = Nothing,
      _tlsVerify = Nothing
    }

--------------------------------------------------------------------------------

-- TODO test force fetch
runPrefetchRule :: NixFetcher Fresh -> ReaderT ActionQueue IO (Maybe Checksum)
runPrefetchRule = runPrefetchRule' _sha256

runPrefetchRule' :: (NixFetcher Fetched -> a) -> NixFetcher Fresh -> ReaderT ActionQueue IO (Maybe a)
runPrefetchRule' g f = runActionChan $ g <$> prefetch f NoForceFetch
