{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module NixExprSpec where

import NeatInterpolation (trimming)
import NvFetcher.NixExpr
import NvFetcher.NixFetcher
import NvFetcher.Types
import PrefetchSpec
import Test.Hspec

spec :: Spec
spec = describe "toNixExpr" $ do
  it "works on bool" $ do
    toNixExpr True `shouldBe` "true"
    toNixExpr False `shouldBe` "false"

  it "works on string" $
    toNixExpr ("Foo" :: String) `shouldBe` [trimming|"Foo"|]

  it "works on list of strings" $
    toNixExpr ["Alice" :: String, "Bob", "Carol"] `shouldBe` [trimming|[ "Alice" "Bob" "Carol" ]|]

  it "renders fresh gitFetcher" $
    toNixExpr (fakeFetch (gitFetcher "https://example.com" "fake_rev"))
      `shouldBe` [trimming|
      fetchgit {
        url = "https://example.com";
        rev = "fake_rev";
        fetchSubmodules = true;
        deepClone = false;
        leaveDotGit = false;
        sparseCheckout = [ ];
        sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
      }
    |]

  it "renders fresh gitHubFetcher" $
    toNixExpr (fakeFetch (gitHubFetcher ("owner", "repo") "fake_rev"))
      `shouldBe` [trimming|
      fetchFromGitHub {
        owner = "owner";
        repo = "repo";
        rev = "fake_rev";
        fetchSubmodules = false;
        sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
      }
    |]

  it "renders fresh urlFetcher" $
    toNixExpr (fakeFetch (urlFetcher "https://example.com"))
      `shouldBe` [trimming|
      fetchurl {
        url = "https://example.com";
        sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
      }
    |]

  it "renders filename for vsc extension" $
    toNixExpr (fakeFetch (openVsxFetcher ("publisher", "extension") "fake_version"))
      `shouldBe` [trimming|
      fetchurl {
        url = "https://open-vsx.org/api/publisher/extension/fake_version/file/publisher.extension-fake_version.vsix";
        name = "extension-fake_version.zip";
        sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
      }
    |]

  it "renders fresh FetchDocker" $ do
    toNixExpr (fakeFetch testDockerFetcher)
      `shouldBe` [trimming|
      dockerTools.pullImage {
        imageName = "library/alpine";
        imageDigest = "sha256:0000000000000000000000000000000000000000000000000000000000000000";
        sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
        finalImageTag = "3.16.2";
      }
      |]

fakeFetch :: NixFetcher Fresh -> NixFetcher Fetched
fakeFetch = \case
  FetchGit {..} -> FetchGit {_sha256 = fakeSha256, ..}
  FetchGitHub {..} -> FetchGitHub {_sha256 = fakeSha256, ..}
  FetchUrl {..} -> FetchUrl {_sha256 = fakeSha256, ..}
  FetchTarball {..} -> FetchTarball {_sha256 = fakeSha256, ..}
  FetchDocker {..} -> FetchDocker {_sha256 = fakeSha256, _imageDigest = fakeDigest, ..}
  where
    fakeSha256 = Checksum "0000000000000000000000000000000000000000000000000000000000000000"
    fakeDigest = ContainerDigest "sha256:0000000000000000000000000000000000000000000000000000000000000000"
