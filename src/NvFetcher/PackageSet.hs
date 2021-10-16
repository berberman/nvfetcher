{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    sourceCmd,

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
    pinned,

    -- ** Miscellaneous
    Prod,
    Append,
    Member,
    OptionalMember,
    NotElem,
    Members,
    OptionalMembers,
    Attach,
    AttachMany,
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
  UseStaleVersion ->
  PackageSet ()
newPackage name source fetcher extract cargo pasthru useStale =
  liftF $ NewPackage (Package name source fetcher extract cargo pasthru useStale) ()

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

-- | A list of 'Member'
type family Members xs r :: Constraint where
  Members '[] _ = ()
  Members (x ': xs) r = (Member x r, Members xs r)

-- | A list of 'OptionalMember'
type family OptionalMembers xs r :: Constraint where
  OptionalMembers '[] _ = ()
  OptionalMembers (x ': xs) r = (OptionalMember x r, OptionalMembers xs r)

-- | @xs ++ ys@, at type level
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Attach members @xs@, with a function argument @arg@
type AttachMany xs arg = forall r. PackageSet (Prod r) -> arg -> PackageSet (Prod (Append xs r))

-- | Attach member @x@, with a function @arg@
type Attach x arg = AttachMany '[x] arg

--------------------------------------------------------------------------------

-- | A tagless final style DSL for constructing packages
class PkgDSL f where
  new :: f PackageName -> f (Prod '[PackageName])
  andThen :: f (Prod r) -> f a -> f (Prod (a ': r))
  end ::
    ( Members
        '[ PackageName,
           VersionSource,
           PackageFetcher
         ]
        r,
      OptionalMembers
        '[ PackageExtractSrc,
           PackageCargoFilePath,
           NvcheckerOptions,
           PackagePassthru,
           UseStaleVersion
         ]
        r
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
      (fromMaybe (UseStaleVersion False) (projMaybe p))

-- | 'PkgDSL' version of 'newPackage'
--
-- Example:
--
-- @
-- define $ package "nvfetcher-git" `sourceGit` "https://github.com/berberman/nvfetcher" `fetchGitHub` ("berberman", "nvfetcher")
-- @
define ::
  ( Members
      '[ PackageName,
         VersionSource,
         PackageFetcher
       ]
      r,
    OptionalMembers
      '[ PackageExtractSrc,
         PackageCargoFilePath,
         PackagePassthru,
         NvcheckerOptions,
         UseStaleVersion
       ]
      r
  ) =>
  PackageSet (Prod r) ->
  PackageSet ()
define = end

-- | Start chaining with the name of package to define
package :: PackageName -> PackageSet (Prod '[PackageName])
package = new . pure

-- | Attach version sources
src :: Attach VersionSource VersionSource
src = (. pure) . andThen

-- | Attach fetchers
fetch :: Attach PackageFetcher PackageFetcher
fetch = (. pure) . andThen

--------------------------------------------------------------------------------

-- | A synonym of 'fetchGitHub' and 'sourceGitHub'
fromGitHub :: AttachMany '[PackageFetcher, VersionSource] (Text, Text)
fromGitHub e (owner, repo) = fromGitHub' e (owner, repo, id)

-- | A synonym of 'fetchGitHub'' and 'sourceGitHub'
fromGitHub' :: AttachMany '[PackageFetcher, VersionSource] (Text, Text, NixFetcher Fresh -> NixFetcher Fresh)
fromGitHub' e p@(owner, repo, _) = fetchGitHub' (sourceGitHub e (owner, repo)) p

-- | A synonym of 'fetchGitHub' and 'sourceGitHubTag'
fromGitHubTag :: AttachMany '[PackageFetcher, VersionSource] (Text, Text, ListOptions -> ListOptions)
fromGitHubTag e (owner, repo, f) = fromGitHubTag' e (owner, repo, f, id)

-- | A synonym of 'fetchGitHub'' and 'sourceGitHubTag'
fromGitHubTag' ::
  AttachMany
    '[PackageFetcher, VersionSource]
    (Text, Text, ListOptions -> ListOptions, NixFetcher Fresh -> NixFetcher Fresh)
fromGitHubTag' e (owner, repo, fv, ff) = fetchGitHub' (sourceGitHubTag e (owner, repo, fv)) (owner, repo, ff)

-- | A synonym of 'fetchPypi' and 'sourcePypi'
fromPypi :: AttachMany '[PackageFetcher, VersionSource] Text
fromPypi e p = fetchPypi (sourcePypi e p) p

-- | A synonym of 'fetchOpenVsx', 'sourceOpenVsx', and 'passthru' extension's publisher with name
fromOpenVsx :: AttachMany '[PackagePassthru, PackageFetcher, VersionSource] (Text, Text)
fromOpenVsx e x@(publisher, extName) =
  passthru
    (fetchOpenVsx (sourceOpenVsx e x) x)
    [ ("name", extName),
      ("publisher", publisher)
    ]

-- | A synonym of 'fetchVscodeMarketplace', 'sourceVscodeMarketplace', and 'passthru' extension's publisher with name
fromVscodeMarketplace :: AttachMany '[PackagePassthru, PackageFetcher, VersionSource] (Text, Text)
fromVscodeMarketplace e x@(publisher, extName) =
  passthru
    (fetchVscodeMarketplace (sourceVscodeMarketplace e x) x)
    [ ("name", extName),
      ("publisher", publisher)
    ]

--------------------------------------------------------------------------------

-- | This package follows the latest github release
sourceGitHub :: Attach VersionSource (Text, Text)
sourceGitHub e (owner, repo) = src e $ GitHubRelease owner repo

-- | This package follows the a tag from github
--
-- Args are owner, repo, and nvchecker list options to find the target tag
sourceGitHubTag :: Attach VersionSource (Text, Text, ListOptions -> ListOptions)
sourceGitHubTag e (owner, repo, f) = src e $ GitHubTag owner repo $ f def

-- | This package follows the latest git commit
--
-- Arg is git url
sourceGit :: Attach VersionSource Text
sourceGit e _vurl = src e $ Git _vurl def

-- | Similar to 'sourceGit', but allows to specify branch
--
-- Args are git url and branch
sourceGit' :: Attach VersionSource (Text, Text)
sourceGit' e (_vurl, coerce . Just -> _vbranch) = src e $ Git {..}

-- | This package follows the latest pypi release
--
-- Arg is pypi name
sourcePypi :: Attach VersionSource Text
sourcePypi e _pypi = src e Pypi {..}

-- | This package follows the version of an Arch Linux package
--
-- Arg is package name in Arch Linux repo
sourceArchLinux :: Attach VersionSource Text
sourceArchLinux e _archpkg = src e ArchLinux {..}

-- | This package follows the version of an Aur package
--
-- Arg is package name in Aur
sourceAur :: Attach VersionSource Text
sourceAur e _aur = src e Aur {..}

-- | This package follows a pinned version
--
-- Arg is manual version
sourceManual :: Attach VersionSource Text
sourceManual e _manual = src e Manual {..}

-- | This package follows the version of a repology package
--
-- Args are repology project name and repo
sourceRepology :: Attach VersionSource (Text, Text)
sourceRepology e (project, repo) = src e $ Repology project repo

-- | This package follows a version extracted from web page
--
-- Args are web page url, regex, and list options
sourceWebpage :: Attach VersionSource (Text, Text, ListOptions -> ListOptions)
sourceWebpage e (_vurl, _regex, f) = src e $ Webpage _vurl _regex $ f def

-- | This package follows a version extracted from http header
--
-- Args are the url of the http request, regex, and list options
sourceHttpHeader :: Attach VersionSource (Text, Text, ListOptions -> ListOptions)
sourceHttpHeader e (_vurl, _regex, f) = src e $ HttpHeader _vurl _regex $ f def

-- | This package follows a version in Open VSX
--
-- Args are publisher and extension name
sourceOpenVsx :: Attach VersionSource (Text, Text)
sourceOpenVsx e (_ovPublisher, _ovExtName) = src e OpenVsx {..}

-- | This package follows a version in Vscode Marketplace
--
-- Args are publisher and extension name
sourceVscodeMarketplace :: Attach VersionSource (Text, Text)
sourceVscodeMarketplace e (_vsmPublisher, _vsmExtName) = src e VscodeMarketplace {..}

-- | This package follows a version from a shell command
--
-- Arg is the command to run
sourceCmd :: Attach VersionSource Text
sourceCmd e _vcmd = src e Cmd {..}

--------------------------------------------------------------------------------

-- | This package is fetched from a github repo
--
-- Args are owner and repo
fetchGitHub :: Attach PackageFetcher (Text, Text)
fetchGitHub e (owner, repo) = fetchGitHub' e (owner, repo, id)

-- | This package is fetched from a github repo
--
-- Similar to 'fetchGitHub', but allows a modifier to the fetcher.
-- For example, you can enable fetch submodules like:
--
-- @
-- define $ package "qliveplayer" `sourceGitHub` ("THMonster", "QLivePlayer") `fetchGitHub'` ("THMonster", "QLivePlayer", fetchSubmodules .~ True)
-- @
fetchGitHub' :: Attach PackageFetcher (Text, Text, NixFetcher Fresh -> NixFetcher Fresh)
fetchGitHub' e (owner, repo, f) = fetch e $ f . gitHubFetcher (owner, repo)

-- | This package is fetched from a file in github release
--
-- Args are owner, repo, and file name
fetchGitHubRelease :: Attach PackageFetcher (Text, Text, Text)
fetchGitHubRelease e (owner, repo, fp) = fetch e $ gitHubReleaseFetcher (owner, repo) fp

-- | This package is fetched from pypi
--
-- Arg is pypi name
fetchPypi :: Attach PackageFetcher Text
fetchPypi e = fetch e . pypiFetcher

-- | This package is fetched from git
--
-- Arg is git url
fetchGit :: Attach PackageFetcher Text
fetchGit e u = fetchGit' e (u, id)

-- | This package is fetched from git
--
-- Similar to 'fetchGit', but allows a modifier to the fetcher.
-- See 'fetchGitHub'' for a concret example.
fetchGit' :: Attach PackageFetcher (Text, NixFetcher Fresh -> NixFetcher Fresh)
fetchGit' e (u, f) = fetch e $ f . gitFetcher u

-- | This package is fetched from url
--
-- Arg is a function which constructs the url from a version
fetchUrl :: Attach PackageFetcher (Version -> Text)
fetchUrl e f = fetch e (urlFetcher . f)

-- | This package is fetched from Open VSX
--
-- Args are publisher and extension name
fetchOpenVsx :: Attach PackageFetcher (Text, Text)
fetchOpenVsx e = fetch e . vscodeMarketplaceFetcher

-- | This package is fetched from Vscode Marketplace
--
-- Args are publisher and extension name
fetchVscodeMarketplace :: Attach PackageFetcher (Text, Text)
fetchVscodeMarketplace e = fetch e . vscodeMarketplaceFetcher

--------------------------------------------------------------------------------

-- | Extract files from fetched package source
extractSource :: Attach PackageExtractSrc [FilePath]
extractSource = (. pure . PackageExtractSrc . NE.fromList) . andThen

-- | Run 'FetchRustGitDependencies' given the path to @Cargo.lock@
--
-- The lock file will be extracted as well.
hasCargoLock :: Attach PackageCargoFilePath FilePath
hasCargoLock = (. pure . PackageCargoFilePath) . andThen

-- | Set 'NvcheckerOptions' for a package, which can tweak the version number we obtain
tweakVersion :: Attach NvcheckerOptions (NvcheckerOptions -> NvcheckerOptions)
tweakVersion = (. pure . ($ def)) . andThen

-- | An attrs set to pass through
--
-- Arg is a list of kv pairs
passthru :: Attach PackagePassthru [(Text, Text)]
passthru = (. pure . PackagePassthru . HMap.fromList) . andThen

-- | Pin a package
--
-- new version won't be checked if we have a stale version
pinned :: PackageSet (Prod r) -> PackageSet (Prod (UseStaleVersion : r))
pinned = flip andThen . pure $ UseStaleVersion True
