{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PrefetchSpec where

import NvFetcher.NixFetcher
import NvFetcher.Types
import Test.Hspec
import Utils

spec :: Spec
spec = aroundActionQueue $
  describe "fetchers" $ do
    specify "pypi" $ \chan ->
      enqueuePrefetch chan (pypiFetcher "example" "0.1.0")
        `shouldReturn` Checksum "1fy3zylvks372jz7fkgad3x14bscz19m2anlq2bn8xs1r1p3x1zm"

    specify "openvsx" $ \chan ->
      enqueuePrefetch chan (openVsxFetcher ("usernamehw", "indent-one-space") "0.2.6")
        `shouldReturn` Checksum "0smb75z10h1aiqmj2irn2vz1z99djfsfpkczwiqk447frx388bd1"
