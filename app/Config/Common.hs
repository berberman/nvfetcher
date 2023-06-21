{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Common where

import Data.Text (Text)
import qualified Data.Text as T
import TOML

githubDecoder :: Decoder (Text, Text)
githubDecoder = makeDecoder $ \case
  v@(String s) -> case T.split (== '/') s of
    [owner, repo] -> pure (owner, repo)
    _ -> invalidValue "unexpected github format: it should be in the format of [owner]/[repo]" v
  v -> typeMismatch v

vscodeExtensionDecoder :: Decoder (Text, Text)
vscodeExtensionDecoder = makeDecoder $ \case
  -- assume that we can't have '.' in extension's name
  v@(String s) -> case T.split (== '.') s of
    [publisher, extName] -> pure (publisher, extName)
    _ -> invalidValue "unexpected vscode extension format: it should be in the format of [publisher].[extName]" v
  v -> typeMismatch v
