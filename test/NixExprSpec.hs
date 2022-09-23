{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NixExprSpec where

import NeatInterpolation (trimming)
import NvFetcher.NixExpr
import NvFetcher.NixFetcher
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
    toNixExpr (gitFetcher "https://example.com" "fake_rev")
      `shouldBe` [trimming|
      fetchgit {
        url = "https://example.com";
        rev = "fake_rev";
        fetchSubmodules = false;
        deepClone = false;
        leaveDotGit = false;
        sha256 = lib.fakeSha256;
      }
    |]

  it "renders fresh gitHubFetcher" $
    toNixExpr (gitHubFetcher ("owner", "repo") "fake_rev")
      `shouldBe` [trimming|
      fetchFromGitHub ({
        owner = "owner";
        repo = "repo";
        rev = "fake_rev";
        fetchSubmodules = false;
        sha256 = lib.fakeSha256;
      })
    |]

  it "renders fresh urlFetcher" $
    toNixExpr (urlFetcher "https://example.com")
      `shouldBe` [trimming|
      fetchurl {
        url = "https://example.com";
        sha256 = lib.fakeSha256;
      }
    |]

  it "renders filename for vsc extension" $
    toNixExpr (openVsxFetcher ("publisher", "extension") "fake_version")
      `shouldBe` [trimming|
      fetchurl {
        url = "https://open-vsx.org/api/publisher/extension/fake_version/file/publisher.extension-fake_version.vsix";
        name = "extension-fake_version.zip";
        sha256 = lib.fakeSha256;
      }
    |]
