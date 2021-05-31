{-# LANGUAGE OverloadedStrings #-}

module Config.ExtractFiles where

import Data.Maybe (fromMaybe)
import Toml

extractFilesCodec :: TomlCodec [FilePath]
extractFilesCodec = dimap Just (fromMaybe []) $ dioptional $ arrayOf _String "extract"
