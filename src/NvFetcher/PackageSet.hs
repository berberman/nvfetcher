{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
--
-- This module mainly contains two things: 'PackageSet' and 'PkgDSL'.
-- NvFetcher accepts the former one -- a set of packages to produce nix sources expr;
-- the later one is used to construct a single package.
--
-- There are many combinators for defining packages. See the documentation of 'define' for example.
module NvFetcher.PackageSet
  ( -- * Package set
    PackageSetF,
    PackageSet,
    newPackage,
    purePackageSet,
    runPackageSet,

    -- * Package DSL

    -- ** Primitives
    PkgDSL (..),
    define,
    package,
    src,
    fetch,

    -- ** Two-in-one functions
    fromGitHub,
    fromGitHub',
    fromGitHubTag,
    fromGitHubTag',
    fromPypi,
    fromOpenVsx,
    fromVscodeMarketplace,

    -- ** Version sources
    sourceGitHub,
    sourceGitHubTag,
    sourceGit,
    sourceGit',
    sourcePypi,
    sourceAur,
    sourceArchLinux,
    sourceManual,
    sourceRepology,
    sourceWebpage,
    sourceHttpHeader,
    sourceOpenVsx,
    sourceVscodeMarketplace,

    -- ** Fetchers
    fetchGitHub,
    fetchGitHub',
    fetchGitHubRelease,
    fetchPypi,
    fetchGit,
    fetchGit',
    fetchUrl,
    fetchOpenVsx,
    fetchVscodeMarketplace,

    -- * Addons
    extractSource,
    hasCargoLock,
    tweakVersion,
    passthru,

    -- ** Miscellaneous
    Prod,
    Member,
    OptionalMember,
    NotElem,
    coerce,
    liftIO,

    -- * Lenses
    (&),
    (.~),
    (%~),
    (^.),
    (?~),
    module NvFetcher.Types.Lens,
  )
where

import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Default (def)
import qualified Data.HashMap.Strict as HMap
import Data.Kind (Constraint, Type)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import GHC.TypeLits
import Lens.Micro
import NvFetcher.NixFetcher
import NvFetcher.Types
import NvFetcher.Types.Lens

--------------------------------------------------------------------------------

-- | Atomic terms of package set
data PackageSetF f
  = NewPackage !Package f
  | forall a. EmbedIO !(IO a) (a -> f)

instance Functor PackageSetF where
  fmap f (NewPackage p g) = NewPackage p $ f g
  fmap f (EmbedIO action g) = EmbedIO action $ f <$> g

-- | Package set is a monad equipped with two capabilities:
--
-- 1. Carry defined packages
-- 2. Run IO actions
--
-- Package set is evaluated be for shake runs.
-- Use 'newPackage' to add a new package, 'liftIO' to run an IO action.
type PackageSet = Free PackageSetF

instance MonadIO PackageSet where
  liftIO io = liftF $ EmbedIO io id

-- | Add a package to package set
newPackage ::
  PackageName ->
  NvcheckerQ ->
  PackageFetcher ->
  Maybe PackageExtractSrc ->
  Maybe PackageCargoFilePath ->
  PackagePassthru ->
  PackageSet ()
newPackage name source fetcher extract cargo pasthru = liftF $ NewPackage (Package name source fetcher extract cargo pasthru) ()

-- | Add a list of packages into package set
purePackageSet :: [Package] -> PackageSet ()
purePackageSet = mapM_ (liftF . flip NewPackage ())

-- | Run package set into a set of packages
--
-- Throws exception as more then one packages with the same name
-- are defined
runPackageSet :: PackageSet () -> IO (Map PackageKey Package)
runPackageSet = \case
  Free (NewPackage p g) ->
    runPackageSet g >>= \m ->
      if isJust (Map.lookup (PackageKey $ _pname p) m)
        then fail $ "Duplicate package name: " <> show (_pname p)
        else pure $ Map.insert (PackageKey $ _pname p) p m
  Free (EmbedIO action g) -> action >>= runPackageSet . g
  Pure _ -> pure mempty

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

instance TypeError (ShowType x :<>: 'Text " is undefined") => Member x '[] where
  proj = undefined

-- | Project optional elements from 'Prod'
class OptionalMember (a :: Type) (r :: [Type]) where
  projMaybe :: Prod r -> Maybe a

instance {-# OVERLAPPING #-} NotElem x xs => OptionalMember x (x ': xs) where
  projMaybe (Cons x _) = Just x

instance OptionalMember x xs => OptionalMember x (_y ': xs) where
  projMaybe (Cons _ r) = projMaybe r

instance OptionalMember x '[] where
  projMaybe Nil = Nothing

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
  end ::
    ( Member PackageName r,
      Member VersionSource r,
      Member PackageFetcher r,
      OptionalMember PackageExtractSrc r,
      OptionalMember PackageCargoFilePath r,
      OptionalMember NvcheckerOptions r,
      OptionalMember PackagePassthru r
    ) =>
    f (Prod r) ->
    f ()

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
    newPackage
      (proj p)
      (NvcheckerQ (proj p) (fromMaybe def (projMaybe p)))
      (proj p)
      (projMaybe p)
      (projMaybe p)
      (fromMaybe mempty (projMaybe p))

-- | 'PkgDSL' version of 'newPackage'
--
-- Example:
--
-- @
-- define $ package "nvfetcher-git" `sourceGit` "https://github.com/berberman/nvfetcher" `fetchGitHub` ("berberman", "nvfetcher")
-- @
define ::
  ( Member PackageName r,
    Member VersionSource r,
    Member PackageFetcher r,
    OptionalMember PackageExtractSrc r,
    OptionalMember PackageCargoFilePath r,
    OptionalMember PackagePassthru r,
    OptionalMember NvcheckerOptions r
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
fromGitHub e (owner, repo) = fromGitHub' e (owner, repo, id)

-- | A synonym of 'fetchGitHub'' and 'sourceGitHub'
fromGitHub' ::
  PackageSet (Prod r) ->
  (Text, Text, NixFetcher Fresh -> NixFetcher Fresh) ->
  PackageSet
    (Prod (PackageFetcher : VersionSource : r))
fromGitHub' e p@(owner, repo, _) = fetchGitHub' (sourceGitHub e (owner, repo)) p

-- | A synonym of 'fetchGitHub' and 'sourceGitHubTag'
fromGitHubTag ::
  PackageSet (Prod r) ->
  (Text, Text, ListOptions -> ListOptions) ->
  PackageSet
    (Prod (PackageFetcher : VersionSource : r))
fromGitHubTag e (owner, repo, f) = fromGitHubTag' e (owner, repo, f, id)

-- | A synonym of 'fetchGitHub'' and 'sourceGitHubTag'
fromGitHubTag' ::
  PackageSet (Prod r) ->
  (Text, Text, ListOptions -> ListOptions, NixFetcher Fresh -> NixFetcher Fresh) ->
  PackageSet
    (Prod (PackageFetcher : VersionSource : r))
fromGitHubTag' e (owner, repo, fv, ff) = fetchGitHub' (sourceGitHubTag e (owner, repo, fv)) (owner, repo, ff)

-- | A synonym of 'fetchPypi' and 'sourcePypi'
fromPypi ::
  PackageSet (Prod r) ->
  Text ->
  PackageSet
    (Prod (PackageFetcher : VersionSource : r))
fromPypi e p = fetchPypi (sourcePypi e p) p

-- | A synonym of 'fetchOpenVsx', 'sourceOpenVsx', and 'passthru' extension's publisher with name
fromOpenVsx ::
  PackageSet (Prod r) ->
  (Text, Text) ->
  PackageSet
    (Prod (PackagePassthru : PackageFetcher : VersionSource : r))
fromOpenVsx e x@(publisher, extName) =
  passthru
    (fetchOpenVsx (sourceOpenVsx e x) x)
    [ ("name", extName),
      ("publisher", publisher)
    ]

-- | A synonym of 'fetchVscodeMarketplace', 'sourceVscodeMarketplace', and 'passthru' extension's publisher with name
fromVscodeMarketplace ::
  PackageSet (Prod r) ->
  (Text, Text) ->
  PackageSet
    (Prod (PackagePassthru : PackageFetcher : VersionSource : r))
fromVscodeMarketplace e x@(publisher, extName) =
  passthru
    (fetchVscodeMarketplace (sourceVscodeMarketplace e x) x)
    [ ("name", extName),
      ("publisher", publisher)
    ]

--------------------------------------------------------------------------------

-- | This package follows the latest github release
sourceGitHub ::
  PackageSet (Prod r) ->
  -- | owner and repo
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceGitHub e (owner, repo) = src e $ GitHubRelease owner repo

-- | This package follows the a tag from github
sourceGitHubTag ::
  PackageSet (Prod r) ->
  -- | owner, repo, and nvchecker list options to find the target tag
  (Text, Text, ListOptions -> ListOptions) ->
  PackageSet (Prod (VersionSource : r))
sourceGitHubTag e (owner, repo, f) = src e $ GitHubTag owner repo $ f def

-- | This package follows the latest git commit
sourceGit ::
  PackageSet (Prod r) ->
  -- | git url
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceGit e _vurl = src e $ Git _vurl def

-- | Similar to 'sourceGit', but allows to specify branch
sourceGit' ::
  PackageSet (Prod r) ->
  -- | git url and branch
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceGit' e (_vurl, coerce . Just -> _vbranch) = src e $ Git {..}

-- | This package follows the latest pypi release
sourcePypi ::
  PackageSet (Prod r) ->
  -- | pypi name
  Text ->
  PackageSet (Prod (VersionSource : r))
sourcePypi e _pypi = src e Pypi {..}

-- | This package follows the version of an Arch Linux package
sourceArchLinux ::
  PackageSet (Prod r) ->
  -- | package name in Arch Linux repo
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceArchLinux e _archpkg = src e ArchLinux {..}

-- | This package follows the version of an Aur package
sourceAur ::
  PackageSet (Prod r) ->
  -- | package name in Aur
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceAur e _aur = src e Aur {..}

-- | This package follows a pinned version
sourceManual ::
  PackageSet (Prod r) ->
  Text ->
  PackageSet (Prod (VersionSource : r))
sourceManual e _manual = src e Manual {..}

-- | This package follows the version of a repology package
sourceRepology ::
  PackageSet (Prod r) ->
  -- | repology project name and repo
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceRepology e (project, repo) = src e $ Repology project repo

-- | This package follows a version extracted from web page
sourceWebpage ::
  PackageSet (Prod r) ->
  -- | web page url, regex, and list options
  (Text, Text, ListOptions -> ListOptions) ->
  PackageSet (Prod (VersionSource : r))
sourceWebpage e (_vurl, _regex, f) = src e $ Webpage _vurl _regex $ f def

-- | This package follows a version extracted from http header
sourceHttpHeader ::
  PackageSet (Prod r) ->
  -- | url of the http request, regex, and list options
  (Text, Text, ListOptions -> ListOptions) ->
  PackageSet (Prod (VersionSource : r))
sourceHttpHeader e (_vurl, _regex, f) = src e $ HttpHeader _vurl _regex $ f def

-- | This package follows a version in Open VSX
sourceOpenVsx ::
  PackageSet (Prod r) ->
  -- | publisher and extension name
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceOpenVsx e (_ovPublisher, _ovExtName) = src e OpenVsx {..}

-- | This package follows a version in Vscode Marketplace
sourceVscodeMarketplace ::
  PackageSet (Prod r) ->
  -- | publisher and extension name
  (Text, Text) ->
  PackageSet (Prod (VersionSource : r))
sourceVscodeMarketplace e (_vsmPublisher, _vsmExtName) = src e VscodeMarketplace {..}

--------------------------------------------------------------------------------

-- | This package is fetched from a github repo
fetchGitHub ::
  PackageSet (Prod r) ->
  -- | owner and repo
  (Text, Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchGitHub e (owner, repo) = fetchGitHub' e (owner, repo, id)

-- | This package is fetched from a github repo
--
-- Similar to 'fetchGitHub', but allows a modifier to the fetcher.
-- For example, you can enable fetch submodules like:
--
-- @
-- define $ package "qliveplayer" `sourceGitHub` ("IsoaSFlus", "QLivePlayer") `fetchGitHub'` ("IsoaSFlus", "QLivePlayer", fetchSubmodules .~ True)
-- @
fetchGitHub' ::
  PackageSet (Prod r) ->
  -- | owner and repo
  ( Text,
    Text,
    NixFetcher Fresh -> NixFetcher Fresh
  ) ->
  PackageSet (Prod (PackageFetcher : r))
fetchGitHub' e (owner, repo, f) = fetch e $ f . gitHubFetcher (owner, repo)

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
fetchGit e u = fetchGit' e (u, id)

-- | This package is fetched from git
--
-- Similar to 'fetchGit', but allows a modifier to the fetcher.
-- See 'fetchGitHub'' for a concret example.
fetchGit' ::
  PackageSet (Prod r) ->
  -- | git url
  (Text, NixFetcher Fresh -> NixFetcher Fresh) ->
  PackageSet (Prod (PackageFetcher : r))
fetchGit' e (u, f) = fetch e $ f . gitFetcher u

-- | This package is fetched from url
fetchUrl ::
  PackageSet (Prod r) ->
  -- | url, given a specific version
  (Version -> Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchUrl e f = fetch e (urlFetcher . f)

-- | This package is fetched from Open VSX
fetchOpenVsx ::
  PackageSet (Prod r) ->
  -- | publisher and extension name
  (Text, Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchOpenVsx e = fetch e . vscodeMarketplaceFetcher

-- | This package is fetched from Vscode Marketplace
fetchVscodeMarketplace ::
  PackageSet (Prod r) ->
  -- | publisher and extension name
  (Text, Text) ->
  PackageSet (Prod (PackageFetcher : r))
fetchVscodeMarketplace e = fetch e . vscodeMarketplaceFetcher

--------------------------------------------------------------------------------

-- | Extract files from fetched package source
extractSource ::
  PackageSet (Prod r) ->
  [FilePath] ->
  PackageSet (Prod (PackageExtractSrc : r))
extractSource = (. pure . PackageExtractSrc . NE.fromList) . andThen

-- | Run 'FetchRustGitDependencies' given the path to @Cargo.lock@
--
-- The lock file will be extracted as well.
hasCargoLock ::
  PackageSet (Prod r) ->
  FilePath ->
  PackageSet (Prod (PackageCargoFilePath : r))
hasCargoLock = (. pure . PackageCargoFilePath) . andThen

-- | Set 'NvcheckerOptions' for a package, which can tweak the version number we obtain
tweakVersion ::
  PackageSet (Prod r) ->
  (NvcheckerOptions -> NvcheckerOptions) ->
  PackageSet (Prod (NvcheckerOptions : r))
tweakVersion = (. pure . ($ def)) . andThen

-- | An attrs set to pass through
passthru ::
  PackageSet (Prod r) ->
  -- | kv list
  [(Text, Text)] ->
  PackageSet (Prod (PackagePassthru : r))
passthru = (. pure . PackagePassthru . HMap.fromList) . andThen
