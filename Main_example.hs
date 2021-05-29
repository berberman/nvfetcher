{-# LANGUAGE OverloadedStrings #-}

module Main where

import NvFetcher

main :: IO ()
main = runNvFetcher defaultArgs {argTarget = "build"} packageSet

packageSet :: PackageSet ()
packageSet = do
  define $ package "gcc-10" `fromGitHubTag` ("gcc-mirror", "gcc", includeRegex ?~ "releases/gcc-10.*")

  define $ package "feeluown-core" `fromPypi` "feeluown"

  define $ package "qliveplayer" `fromGitHub'` ("IsoaSFlus", "QLivePlayer", fetchSubmodules .~ True)

  define $
    package "fcitx5-pinyin-zhwiki"
      `sourceAur` "fcitx5-pinyin-zhwiki"
      `fetchUrl` \v ->
        "https://github.com/felixonmars/fcitx5-pinyin-zhwiki/releases/download/0.2.2/zhwiki-"
          <> coerce v
          <> ".dict"

  define $
    package "apple-emoji"
      `sourceManual` "0.0.0.20200413"
      `fetchUrl` const
        "https://github.com/samuelngs/apple-emoji-linux/releases/download/latest/AppleColorEmoji.ttf"

  define $
    package "nvfetcher-git"
      `sourceGit` "https://github.com/berberman/nvfetcher"
      `fetchGitHub` ("berberman", "nvfetcher")
