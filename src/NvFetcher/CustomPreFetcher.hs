{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | Copyright: (c) 2021-2025 berberman
SPDX-License-Identifier: MIT
Maintainer: berberman <berberman@yandex.com>
Stability: experimental
Portability: portable

This module provides function that extracts files contents from package sources.
Because we use @nix-instantiate@ to build drv, so @<nixpkgs>@ (@NIX_PATH@) is required.
-}
module NvFetcher.CustomPrefetcher (
    -- * Types
    FetchCustomFetcherQ (..),

    -- * Rules
    customPrefetcherRule,

    -- * Functions
    runCustomPrefetcher,
)
where

import qualified Data.HashMap.Strict as HMap

import Control.Exception (ErrorCall)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath ((</>))
import NvFetcher.Config (Config (keepGoing))
import NvFetcher.ExtractSrc (extractSrc)
import NvFetcher.NixExpr (fetcherToExtractDrv)
import NvFetcher.Types
import NvFetcher.Types.ShakeExtras (getBuildDir, withRetry)
import NvFetcher.Utils (quote, quoteAndEscape, quoteIfNeeds)
import qualified NvFetcher.Utils as T
import Prettyprinter (pretty, (<+>))

getFile :: NixFetcher Fetched -> FilePath -> Action (Maybe FilePath)
getFile fetcher file = do
    buildDir <- getBuildDir
    result <- extractSrc fetcher (Glob file)
    case HMap.toList result of
        [(s, fp)] -> pure $ Just s
        _ -> pure Nothing

customPrefetcherRule :: Rules ()
customPrefetcherRule = void $
    addOracle $ \q@(FetchCustomFetcherQ fetcher command file) -> do
        path <- getFile fetcher file
        case path of
            Nothing -> do
                putInfo . show $ "#" <+> pretty q
                putWarn $ "File " <> file <> " not found in the source. Skipping custom prefetcher."
                pure Nothing
            Just path -> do
                putInfo . show $ "#" <+> pretty q
                withTempFile $ \fp -> withRetry $ do
                    putInfo . show $ "#" <+> pretty q
                    let nixExpr = T.unpack $ fetcherToExtractDrv fetcher (T.pack command) path
                    putVerbose $ "Generated nix expr:\n" <> nixExpr
                    writeFile' fp nixExpr
                    (CmdTime t, StdoutTrim out, CmdLine c, Stdouterr err) <- quietly $ cmd $ "nix-build --no-out-link " <> fp
                    putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
                    putVerbose $ "Output from stdout: " <> out
                    putVerbose $ "Output from stderr: " <> err
                    stdout <- liftIO $ T.readFile out
                    if T.null stdout
                        then do
                            putWarn $ "Custom prefetcher " <> command <> " did not return any output. Skipping."
                            pure Nothing
                        else pure . Just $ T.strip stdout

runCustomPrefetcher :: NixFetcher Fetched -> String -> FilePath -> Action (Maybe T.Text)
runCustomPrefetcher fetcher command fp = askOracle $ FetchCustomFetcherQ fetcher command fp
