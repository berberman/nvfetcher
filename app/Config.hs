{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config where

import Config.PackageFetcher
import Config.VersionSource
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Lens.Micro
import Lens.Micro.Extras (view)
import NvFetcher.Types
import Toml
import Validation (validationToEither)

parseConfig :: TOML -> Either [Toml.TomlDecodeError] [Package]
parseConfig toml = go tables [] []
  where
    go (Left errs : xs) se sp = go xs (se <> errs) sp
    go (Right x : xs) se sp = go xs se (x : sp)
    go [] [] sp = Right sp
    go [] se _ = Left se
    tables = [fmap (toPackage k) $ validationToEither $ Toml.runTomlCodec iCodec v | (Toml.unKey -> (Toml.unPiece -> k) :| _, v) <- Toml.toList $ Toml.tomlTables toml]
    toPackage k (v, f, e, c) = Package k v f e c
    iCodec = (,,,) <$> versionSourceCodec .= view _1 <*> fetcherCodec .= view _2 <*> extractFilesCodec .= view _3 <*> cargoLockPathCodec .= view _4

extractFilesCodec :: TomlCodec [FilePath]
extractFilesCodec = dimap Just (fromMaybe []) $ dioptional $ arrayOf _String "extract"

cargoLockPathCodec :: TomlCodec (Maybe FilePath)
cargoLockPathCodec = dioptional (string "cargo_lock")
