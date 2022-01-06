{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config where

import Config.PackageFetcher
import Config.VersionSource
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
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
    tables =
      [ fmap (toPackage (coerce k)) $
          validationToEither $
            Toml.runTomlCodec packageConfigCodec v
        | (Toml.unKey -> (Toml.unPiece -> k) :| _, v) <- Toml.toList $ Toml.tomlTables toml
      ]

--------------------------------------------------------------------------------

data PackageConfig = PackageConfig
  { pcVersionSource :: VersionSource,
    pcFetcher :: PackageFetcher,
    pcExtractFiles :: Maybe PackageExtractSrc,
    pcCargoLockPath :: Maybe PackageCargoFilePath,
    pcNvcheckerOptions :: NvcheckerOptions,
    pcPassthru :: PackagePassthru,
    pcUseStale :: UseStaleVersion
  }

toPackage :: PackageKey -> PackageConfig -> Package
toPackage k PackageConfig {..} =
  Package
    (coerce k)
    (CheckVersion pcVersionSource pcNvcheckerOptions)
    pcFetcher
    pcExtractFiles
    pcCargoLockPath
    pcPassthru
    pcUseStale

packageConfigCodec :: TomlCodec PackageConfig
packageConfigCodec =
  PackageConfig
    <$> versionSourceCodec .= pcVersionSource
    <*> fetcherCodec .= pcFetcher
    <*> extractFilesCodec .= pcExtractFiles
    <*> cargoLockPathCodec .= pcCargoLockPath
    <*> nvcheckerOptionsCodec .= pcNvcheckerOptions
    <*> passthruCodec .= pcPassthru
    <*> pinnedCodec .= pcUseStale

--------------------------------------------------------------------------------

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

passthruCodec :: TomlCodec PackagePassthru
passthruCodec = diwrap $ tableHashMap _KeyText text "passthru"

pinnedCodec :: TomlCodec UseStaleVersion
pinnedCodec = dimap (Just . coerce) (maybe (UseStaleVersion False) coerce) $ dioptional $ bool "pinned"
