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
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
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
    toPackage k (versionSource, f, e, c, options) = Package k (NvcheckerQ versionSource options) f e c
    iCodec =
      (,,,,)
        <$> versionSourceCodec .= view _1
        <*> fetcherCodec .= view _2
        <*> extractFilesCodec .= view _3
        <*> cargoLockPathCodec .= view _4
        <*> nvcheckerOptionsCodec .= view _5

extractFilesCodec :: TomlCodec (Maybe PackageExtractSrc)
extractFilesCodec =
  dimap
    (fmap (NE.toList . coerce))
    (\mxs -> coerce <$> (mxs >>= NE.nonEmpty))
    $ dioptional $ arrayOf _String "extract"

cargoLockPathCodec :: TomlCodec (Maybe PackageCargoFilePath)
cargoLockPathCodec = dioptional $ diwrap (string "cargo_lock")

nvcheckerOptionsCodec :: TomlCodec NvcheckerOptions
nvcheckerOptionsCodec =
  NvcheckerOptions
    <$> dioptional (text "src.prefix") .= _stripPrefix
    <*> dioptional (text "src.from_pattern") .= _fromPattern
    <*> dioptional (text "src.to_pattern") .= _toPattern
