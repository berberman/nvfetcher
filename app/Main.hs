{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Config
import Development.NvFetcher
import System.Console.GetOpt
import qualified Toml

newtype ConfigPath = ConfigPath FilePath deriving (Show)

flags :: [OptDescr (Either String ConfigPath)]
flags = [Option ['c'] ["config"] (ReqArg (Right . ConfigPath) "FILE") "Path to nvfetcher config"]

main :: IO ()
main = defaultMainWith defaultArgs flags $ \flagValues -> do
  let fp = case flagValues of
        [ConfigPath fp] -> fp
        _ -> "nvfetcher.toml"

  e <- Toml.decodeFileEither nvfetcherConfigCodec fp
  case e of
    Left e -> error $ "Failed to parse config: " <> show e
    Right (getPackages -> x) -> pure (purePackageSet x)
