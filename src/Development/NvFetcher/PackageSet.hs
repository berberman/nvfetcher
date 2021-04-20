{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.NvFetcher.PackageSet where

import Control.Monad.Free
import Data.Set (Set)
import qualified Data.Set as Set
import Development.NvFetcher.NixFetcher
import Development.NvFetcher.Types
import Development.Shake (Action)

-- | This is trivial XD
data PackageSetF f
  = NewPackage PackageName VersionSource (Version -> NixFetcher Fresh) f
  | forall a. EmbedAction (Action a) (a -> f)

instance Functor PackageSetF where
  fmap f (NewPackage name src fe g) = NewPackage name src fe $ f g
  fmap f (EmbedAction action g) = EmbedAction action $ f <$> g

type PackageSet = Free PackageSetF

newPackage :: PackageName -> VersionSource -> (Version -> NixFetcher Fresh) -> PackageSet ()
newPackage name src fe = liftF $ NewPackage name src fe ()

embedAction :: Action a -> PackageSet a
embedAction action = liftF $ EmbedAction action id

runPackageSet :: PackageSet a -> Action (Set Package)
runPackageSet = \case
  Free (NewPackage name src fe g) -> (Package name src fe `Set.insert`) <$> runPackageSet g
  Free (EmbedAction action g) -> action >>= runPackageSet . g
  Pure _ -> pure Set.empty
