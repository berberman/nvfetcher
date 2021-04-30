{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module mainly contains two things: 'PackageSet' and 'PkgDSL'.
-- NvFetcher accepts the former one -- a set of packages to produce nix sources expr;
-- the later one is used to construct a single package.
module NvFetcher.PackageSet
  ( -- * Package set
    PackageSet,
    newPackage,
    embedAction,
    purePackageSet,
    runPackageSet,

    -- * Package DSL
    PkgDSL (..),
    define,
    package,
    src,
    fetch,

    -- ** One-stop shops
    fromGitHub,
    fromPypi,

    -- ** Version sources
    sourceGitHub,
    sourceGit,
    sourcePypi,
    sourceAur,
    sourceArchLinux,
    sourceManual,
    sourceRepology,

    -- ** Fetchers
    fetchGitHub,
    fetchGitHubRelease,
    fetchPypi,
    fetchGit,
    fetchUrl,

    -- ** Miscellaneous
    Prod (..),
    Member (..),
    NotElem,
  )
where

import Control.Monad.Free
import Data.Kind (Constraint, Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Development.Shake (Action)
import GHC.TypeLits
import NvFetcher.NixFetcher
import NvFetcher.Types

--------------------------------------------------------------------------------

-- | This is trivial XD
data PackageSetF f
  = NewPackage !PackageName !VersionSource !PackageFetcher f
  | forall a. EmbedAction !(Action a) (a -> f)

instance Functor PackageSetF where
  fmap f (NewPackage name src fe g) = NewPackage name src fe $ f g
  fmap f (EmbedAction action g) = EmbedAction action $ f <$> g

-- | Package set is a free monad equipped with two capabilities:
--
-- 1. Carry defined packages
-- 2. Run shake actions
--
-- Consturct it using 'newPackage' and 'embedAction'
type PackageSet = Free PackageSetF

-- | Add a package to package set
newPackage ::
  PackageName ->
  VersionSource ->
  PackageFetcher ->
  PackageSet ()
newPackage name source fetcher = liftF $ NewPackage name source fetcher ()

-- | Lift a shake 'Action' to package set
embedAction :: Action a -> PackageSet a
embedAction action = liftF $ EmbedAction action id

-- | Add a list of packages into package set
purePackageSet :: [Package] -> PackageSet ()
purePackageSet = mapM_ (\Package {..} -> newPackage pname pversion pfetcher)

-- | Run package set into a set of packages, carried by 'Action'
runPackageSet :: PackageSet () -> Action (Set Package)
runPackageSet = \case
  Free (NewPackage name src fe g) -> (Package name src fe `Set.insert`) <$> runPackageSet g
  Free (EmbedAction action g) -> action >>= runPackageSet . g
  Pure _ -> pure Set.empty

--------------------------------------------------------------------------------

-- | Simple HList
data Prod (r :: [Type]) where
  Nil :: Prod '[]
  Cons :: !x -> Prod xs -> Prod (x ': xs)

-- | Project elements from 'Prod'
class Member (a :: Type) (r :: [Type]) where
  proj :: Prod r -> a

instance {-# OVERLAPPING #-} NotElem x xs => Member x (x ': xs) where
  proj (Cons x _) = x

instance Member x xs => Member x (_y ': xs) where
  proj (Cons _ r) = proj r

instance TypeError (ShowType x :<>: 'Text " is not defined") => Member x '[] where
  proj = undefined

-- | Constraint for producing error messages
type family NotElem (x :: Type) (xs :: [Type]) :: Constraint where
  NotElem x (x ': xs) = TypeError (ShowType x :<>: 'Text " is defined more than one times")
  NotElem x (_ ': xs) = NotElem x xs
  NotElem x '[] = ()

--------------------------------------------------------------------------------

-- | A tagless final style DSL for constructing packages
class PkgDSL f where
  new :: f PackageName -> f (Prod '[PackageName])
  andThen :: f (Prod r) -> f a -> f (Prod (a ': r))
  end :: (Member PackageName r, Member VersionSource r, Member PackageFetcher r) => f (Prod r) -> f ()

instance PkgDSL PackageSet where
  new e = do
    name <- e
    pure $ Cons name Nil
  andThen e e' = do
    p <- e
    x <- e'
    pure $ Cons x p
  end e = do
    p <- e
    newPackage (proj p) (proj p) (proj p)

-- | 'PkgDSL' version of 'newPackage'
--
-- Example:
-- >>> define $ package "nvfetcher-git" `sourceGit` "nvfetcher" `fetchGitHub` ("berberman", "nvfetcher")
define ::
  ( Member PackageName r,
    Member VersionSource r,
    Member PackageFetcher r
  ) =>
  PackageSet (Prod r) ->
  PackageSet ()
define = end

-- | Start chaining with the name of package to define
package :: PackageName -> PackageSet (Prod '[PackageName])
package = new . pure

-- | Attach version sources
src :: PackageSet (Prod r) -> VersionSource -> PackageSet (Prod (VersionSource ': r))
src = (. pure) . andThen

-- | Attach fetchers
fetch ::
  PackageSet (Prod r) ->
  PackageFetcher ->
  PackageSet (Prod (PackageFetcher ': r))
fetch = (. pure) . andThen

--------------------------------------------------------------------------------

-- | A synonym of 'fetchGitHub' and 'sourceGitHub'
fromGitHub ::
  PackageSet (Prod r) ->
  (Text, Text) ->
  PackageSet
    (Prod (PackageFetcher : VersionSource : r))
fromGitHub e p = fetchGitHub (sourceGitHub e p) p

-- | A synonym of 'fetchPypi' and 'sourcePypi'
fromPypi ::
  PackageSet (Prod r) ->
  Text ->
  PackageSet
    (Prod (PackageFetcher : VersionSource : r))
fromPypi e p = fetchPypi (sourcePypi e p) p

--------------------------------------------------------------------------------

-- | This package follows the latest github release
sourceGitHub ::
  PackageSet (Prod r) ->
  -- | owner and repo
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceGitHub e (owner, repo) = src e $ GitHubRelease owner repo

-- | This package follows the latest git commit
sourceGit ::
  PackageSet (Prod r) ->
  -- | git url
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceGit e vurl = src e Git {..}

-- | This package follows the latest pypi release
sourcePypi ::
  PackageSet (Prod r) ->
  -- | pypi name
  Text ->
  PackageSet (Prod (VersionSource : r))
sourcePypi e pypi = src e Pypi {..}

-- | This package follows the version of an Arch Linux package
sourceArchLinux ::
  PackageSet (Prod r) ->
  -- | package name in Arch Linux repo
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceArchLinux e archpkg = src e ArchLinux {..}

-- | This package follows the version of an Aur package
sourceAur ::
  PackageSet (Prod r) ->
  -- | package name in Aur
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceAur e aur = src e Aur {..}

-- | This package follows a pinned version
sourceManual ::
  PackageSet (Prod r) ->
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceManual e manual = src e Manual {..}

-- | This package follows the version of a repology package
sourceRepology ::
  PackageSet (Prod r) ->
  -- | repology project name and repo
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceRepology e (project, repo) = src e $ Repology project repo

--------------------------------------------------------------------------------

-- | This package is fetched from a github repo
fetchGitHub ::
  PackageSet (Prod r) ->
  -- | owner and repo
  (Text, Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchGitHub e p = fetch e $ gitHubFetcher p

-- | This package is fetched from a file in github release
fetchGitHubRelease ::
  PackageSet (Prod r) ->
  -- | owner, repo, and file name
  (Text, Text, Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchGitHubRelease e (owner, repo, fp) = fetch e $ gitHubReleaseFetcher (owner, repo) fp

-- | This package is fetched from pypi
fetchPypi ::
  PackageSet (Prod r) ->
  -- | pypi name
  Text ->
  PackageSet (Prod (PackageFetcher : r))
fetchPypi e = fetch e . pypiFetcher

-- | This package is fetched from git
fetchGit ::
  PackageSet (Prod r) ->
  -- | git url
  Text ->
  PackageSet (Prod (PackageFetcher : r))
fetchGit e = fetch e . gitFetcher

-- | This package is fetched from url
fetchUrl ::
  PackageSet (Prod r) ->
  -- | url, given a specific version
  (Version -> Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchUrl e f = fetch e (urlFetcher . f)

--------------------------------------------------------------------------------
