{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Config (prettyPackageConfigParseError, parseConfig, PackageConfigValidateError (..)) where

import Config.PackageFetcher
import Config.VersionSource
import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HMap
import Data.List (foldl', intersect)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import NvFetcher.Types
import TOML
import TOML.Decode

-- should be NE
newtype MyKey = MyKey {unMyKey :: [Text]}

myKeyToText :: MyKey -> Text
myKeyToText = T.intercalate "." . unMyKey

data PackageConfigValidateError
  = TomlError TOMLError
  | KeyConflicts PackageName [[Text]]
  | KeyUnexpected PackageName [Text]

prettyPackageConfigParseError :: PackageConfigValidateError -> Text
prettyPackageConfigParseError (TomlError e) = "Failed to parse config file: " <> renderTOMLError e
prettyPackageConfigParseError (KeyConflicts pkg xs) = T.unlines ["In [" <> pkg <> "], key conflicts occurred: " <> T.intercalate ", " ks | ks <- xs]
prettyPackageConfigParseError (KeyUnexpected pkg xs) = T.unlines ["In [" <> pkg <> "], unexpected keys found: " <> k | k <- xs]

parseConfig :: Text -> Either [PackageConfigValidateError] [Package]
parseConfig raw = runExcept $ case decodeWith tableDecoder raw of
  Right (Map.toList -> x) -> mapM (uncurry eachP) x
  Left err -> throwE [TomlError err]
  where
    tableDecoder =
      makeDecoder $ \case
        (Table t) -> pure t
        v -> typeMismatch v
    eachP pkg v@(Table _) = do
      let keys = myKeyToText <$> allKeys [] v
      checkConflicts pkg keys
      checkUnexpected pkg keys
      case unDecodeM (runDecoder (packageConfigDecoder pkg) v) [] of
        Left e -> throwE [TomlError $ uncurry DecodeError e]
        Right x -> pure x
    eachP pkg _ = throwE [KeyUnexpected pkg [pkg]]
    allKeys prefix (Table t) =
      mconcat
        [ case x of
            (Table _) -> []
            _ -> [MyKey $ prefix <> [k]]
          | (k, x) <- Map.toList t
        ]
        <> Map.foldrWithKey (\k v acc -> allKeys (prefix <> [k]) v <> acc) [] t
    allKeys _ _ = []
    checkConflicts pkg keys =
      throwN
        [ KeyConflicts pkg [intersection]
          | k <-
              [ ("src." <>) <$> versionSourceKeys,
                ("fetch." <>) <$> fetcherKeys
              ],
            let intersection = keys `intersect` k,
            length intersection > 1
        ]
    checkUnexpected pkg keys =
      throwN $
        -- git
        [ KeyUnexpected pkg gk
          | let gk = filter (T.isPrefixOf "git.") keys,
            not $ null gk,
            "fetch.git" `notElem` keys && "fetch.github" `notElem` keys
        ]
          -- docker
          <> [ KeyUnexpected pkg dk
               | let dk = filter (T.isPrefixOf "docker.") keys,
                 not $ null dk,
                 "fetch.docker" `notElem` keys
             ]
          -- list options
          <> [ KeyUnexpected pkg lk
               | let lk = listOptionsKeys `intersect` keys,
                 not $ null lk,
                 "src.docker" `notElem` keys
                   && "src.httpheader" `notElem` keys
                   && "src.container" `notElem` keys
                   && "src.github_tag" `notElem` keys
             ]
    throwN [] = pure ()
    throwN xs = throwE xs

--------------------------------------------------------------------------------

packageConfigDecoder :: PackageName -> Decoder Package
packageConfigDecoder name =
  Package name
    <$> (CheckVersion <$> versionSourceDecoder <*> nvcheckerOptionsDecoder)
    <*> fetcherDecoder
    <*> extractFilesDecoder
    <*> cargoLockPathDecoder
    <*> passthruDecoder
    <*> pinnedDecoder
    <*> gitDateFormatDecoder
    <*> forceFetchDecoder

--------------------------------------------------------------------------------

extractFilesDecoder :: Decoder (Maybe PackageExtractSrc)
extractFilesDecoder = fmap PackageExtractSrc <$> getFieldOpt "extract"

cargoLockPathDecoder :: Decoder (Maybe PackageCargoLockFiles)
cargoLockPathDecoder = fmap PackageCargoLockFiles <$> getFieldOpt "cargo_locks"

nvcheckerOptionsDecoder :: Decoder NvcheckerOptions
nvcheckerOptionsDecoder =
  NvcheckerOptions
    <$> getFieldsOpt ["src", "prefix"]
    <*> getFieldsOpt ["src", "from_pattern"]
    <*> getFieldsOpt ["src", "to_pattern"]

passthruDecoder :: Decoder PackagePassthru
passthruDecoder =
  getFieldOpt @Value "passthru" >>= \case
    Just (Table t) -> go [] t >>= \(mconcat -> fs) -> pure $ PackagePassthru $ foldl' (flip ($)) HMap.empty fs
    Just _ -> makeDecoder typeMismatch
    Nothing -> pure $ PackagePassthru HMap.empty
  where
    go prefix x =
      sequenceA
        [ case v of
            (String text) -> pure [HMap.insert (myKeyToText $ MyKey $ prefix <> [k]) text]
            Table t -> mconcat <$> go (prefix <> [k]) t
            _ -> makeDecoder (\_ -> invalidValue "passthru value must be string for now" v)
          | (k, v) <- Map.toList x
        ]

pinnedDecoder :: Decoder UseStaleVersion
pinnedDecoder =
  maybe NoStale (\x -> if x then PermanentStale else NoStale)
    <$> getFieldOpt "pinned"

gitDateFormatDecoder :: Decoder DateFormat
gitDateFormatDecoder = DateFormat <$> getFieldsOpt ["git", "date_format"]

forceFetchDecoder :: Decoder ForceFetch
forceFetchDecoder =
  maybe NoForceFetch (\x -> if x then ForceFetch else NoForceFetch)
    <$> getFieldsOpt ["fetch", "force"]
