{-# LANGUAGE OverloadedStrings #-}

module CheckVersionSpec where

import NvFetcher.Nvchecker
import Test.Hspec
import Utils

spec :: Spec
spec = aroundActionQueue $
  describe "nvchecker" $ do
    specify "pypi" $ \chan ->
      enqueueNvchecker chan (Pypi "example") `shouldReturn` "0.1.0"

