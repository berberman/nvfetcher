{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module PrefetchSpec where

import NvFetcher.NixFetcher
import NvFetcher.Types
import Test.Hspec
import Utils

spec :: Spec
spec = aroundShake $
  describe "fetchers" $ do
    specify "pypi" $ \chan ->
      runPrefetchRule chan (pypiFetcher "example" "0.1.0")
        `shouldReturnJust` Checksum "1fy3zylvks372jz7fkgad3x14bscz19m2anlq2bn8xs1r1p3x1zm"

    specify "openvsx" $ \chan ->
      runPrefetchRule chan (openVsxFetcher ("usernamehw", "indent-one-space") "0.2.6")
        `shouldReturnJust` Checksum "0smb75z10h1aiqmj2irn2vz1z99djfsfpkczwiqk447frx388bd1"

    specify "vsmarketplace" $ \chan ->
      runPrefetchRule chan (vscodeMarketplaceFetcher ("usernamehw", "indent-one-space") "0.2.6")
        `shouldReturnJust` Checksum "1vmq24hdbv8jwhvy85s7qq9gffcsndvsw8vmphxs95r7bc3439w7"

    specify "git" $ \chan ->
      runPrefetchRule chan (gitFetcher "https://gitlab.com/gitlab-org/gitlab-test.git" "ddd0f15ae83993f5cb66a927a28673882e99100b")
        `shouldReturnJust` Checksum "10d0yqa0h00pd5a36nwx3hb4apd4f9hki0pi545ffgrpf9nzzg92"

    specify "github" $ \chan ->
      runPrefetchRule chan (gitHubFetcher ("harry-sanabria", "ReleaseTestRepo") "release3")
        `shouldReturnJust` Checksum "1a8qv2h9ji6w5p4ljwwkgvk49w6hj9j7hgmw0ahw10y1i45s0b3i"

--------------------------------------------------------------------------------

runPrefetchRule :: ActionQueue -> NixFetcher Fresh -> IO (Maybe Checksum)
runPrefetchRule chan f = runAction chan $ _sha256 <$> prefetch f
