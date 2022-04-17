{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021-2022 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Lenses for "NvFetcher.Types"
module NvFetcher.Types.Lens where

import Lens.Micro.TH
import NvFetcher.Types

makeLenses ''ListOptions

makeLenses ''NvcheckerOptions

makeLenses ''VersionSource

makeLenses ''NixFetcher

makeLenses ''Package
