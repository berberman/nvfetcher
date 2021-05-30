{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Lenses for "NvFetcher.Types"
module NvFetcher.Types.Lens where

import Lens.Micro.TH
import NvFetcher.Types

makeLenses ''ListOptions

makeLenses ''VersionSource

makeLenses ''NixFetcher

makeLenses ''Package
