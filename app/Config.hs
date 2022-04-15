{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config where

import Config.PackageFetcher
import Config.VersionSource
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft)
import qualified Data.HashMap.Strict as HMap
import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import NvFetcher.Types
import Toml
import Validation (validationToEither)

data PackageConfigParseError = TomlErrors [TomlDecodeError] | KeyConflicts [[Key]]

instance Semigroup PackageConfigParseError where
  TomlErrors e <> _ = TomlErrors e
  _ <> TomlErrors e = TomlErrors e
  KeyConflicts xs <> KeyConflicts ys = KeyConflicts $ xs <> ys

prettyPackageConfigParseError :: PackageConfigParseError -> Text
prettyPackageConfigParseError (TomlErrors e) = prettyTomlDecodeErrors e
prettyPackageConfigParseError (KeyConflicts xs) = "Skip parsing!\n" <> T.unlines ["Key conflict: " <> T.intercalate ", " [prettyKey k | k <- ks] | ks <- xs]

parseConfig :: TOML -> Either PackageConfigParseError [Package]
parseConfig toml = go tables Nothing []
  where
    go (Left errs : xs) (Just se) sp = go xs (Just (se <> errs)) sp
    go (Left errs : xs) Nothing sp = go xs (Just errs) sp
    go (Right x : xs) se sp = go xs se (x : sp)
    go [] Nothing sp = Right sp
    go [] (Just se) _ = Left se
    tables =
      [ fmap (toPackage (coerce k)) $ validateKeys v >> mapLeft TomlErrors (validationToEither (Toml.runTomlCodec packageConfigCodec v))
        | (Toml.unKey -> (Toml.unPiece -> k) :| _, v) <- Toml.toList $ Toml.tomlTables toml
      ]

validateKeys :: TOML -> Either PackageConfigParseError ()
validateKeys toml = if null e then Right () else Left $ foldl1 (<>) e
  where
    allKeys = HMap.keys $ Toml.tomlPairs toml
    go xs = let intersection = xs `intersect` allKeys in if length intersection > 1 then intersection else []
    e = [KeyConflicts [t] | k <- [versionSourceKeys, fetcherKeys], let t = go k, not $ null t]

--------------------------------------------------------------------------------

data PackageConfig = PackageConfig
  { pcVersionSource :: VersionSource,
    pcFetcher :: PackageFetcher,
    pcExtractFiles :: Maybe PackageExtractSrc,
    pcCargoLockFiles :: Maybe PackageCargoLockFiles,
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
    pcCargoLockFiles
    pcPassthru
    pcUseStale

packageConfigCodec :: TomlCodec PackageConfig
packageConfigCodec =
  PackageConfig
    <$> versionSourceCodec .= pcVersionSource
    <*> fetcherCodec .= pcFetcher
    <*> extractFilesCodec .= pcExtractFiles
    <*> cargoLockPathCodec .= pcCargoLockFiles
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

cargoLockPathCodec :: TomlCodec (Maybe PackageCargoLockFiles)
cargoLockPathCodec =
  dimap
    (fmap (NE.toList . coerce))
    (\mxs -> coerce <$> (mxs >>= NE.nonEmpty))
    $ dioptional $ arrayOf _String "cargo_locks"

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
