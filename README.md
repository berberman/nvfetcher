# nvfetcher

nvfetcher is a build system built on top of [shake](https://www.shakebuild.com/),
integrating [nvchecker](https://github.com/lilydjwg/nvchecker).
It's very simple -- the system produces only one artifact as the result of build.
nvfetcher cli program accepts a TOML file as config, which defines a set of package sources to run.

## Overview

For example, given configuration file:

```toml
# nvfetcher.toml
[[package]]
name = "feeluown-core"
src.pypi = "feeluown"
fetch.pypi = "feeluown"

[[package]]
name = "qliveplayer"
src.github = "IsoaSFlus/QLivePlayer"
fetch.github = "IsoaSFlus/QLivePlayer"
```

running `nvfetcher build` will create `sources.nix` like:

```nix
# sources.nix
{ fetchgit, fetchurl }:
{
  feeluown-core = {
    pname = "feeluown-core";
    version = "3.7.6";
    src = fetchurl {
      sha256 = "1bsz149dv3j5sfbynjrqsqbkkxdxkdlq4sdx2vi8whvfwfg0j2f0";
      url = "https://pypi.io/packages/source/f/feeluown/feeluown-3.7.6.tar.gz";
    };
  };
  qliveplayer = {
    pname = "qliveplayer";
    version = "3.22.0";
    src = fetchgit {
      url = "https://github.com/IsoaSFlus/QLivePlayer";
      rev = "3.22.0";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "192g42pvibms2rsjh68fck4bj59b10ay9zqcf2hqhcka0xmnyb09";
    };
  };
}
```

We tell nvfetcher how to get the latest version number of packages and how to fetch their sources given version numbers,
and nvfetcher will help us keep their version and prefetched SHA256 sums up-to-date, stored in `sources.nix`.
Shake will help us handle necessary rebuilds -- we check versions of packages during each run, but only prefetch them when needed.

## Usage

Basically, there are two ways to use nvfetcher, where the difference is how we provide package sources definitions to it. 
No matter which way you use it in, CLI options are inherited from shake with two targets, typically used as:

* `nvfetcher build` - our main purpose, creating `sources.nix`
* `nvfetcher clean` - clean up cache and remove `sources.nix`

> Running `nvfetcher` without any target has no effect!

> You can specify `-j` to enable parallelism

### CLI

To run nvfetcher as a CLI program, you'll need to provide package sources defined in TOML.

Two available CLI options:
* `-c` (`--config`) - path to the TOML configuration file
* `-o` (`--output`) - path to the output nix file

A *package* consists of three parts:
* a name - `name = pkg_name`
* a nvchecker configuration, how to track version updates
  * `src.github = owner/repo` - the latest gituhb release
  * `src.pypi = pypi_name` - the latest pypi release
  * `src.git = git_url` - the latest commit of a repo
  * `src.archpkg = archlinux_pkg_name` -- the latest version of an archlinux package
  * `src.aur = aur_pkg_name` -- the latest version of an aur package
  * `src.manual = v` -- a fixed version, which never updates
  * `src.repology = project:repo` -- the latest version from repology
* a nix fetcher function, how to fetch the package given the version number. `$ver` is available, which will be set to the result of nvchecker.
  * `fetch.github = owner/repo` or `owner/repo:rev` (default to `$ver` if no `rev` specified)
  * `fetch.pypi = pypi_name` or `pypi_name:ver` (default to `$ver` if no `ver` specified)
  * `fetch.git = git_url` or `git_url:rev` (default to `$ver` if no `rev` specified)
  * `fetch.url = url`

Each *package* has three such fields - the entire of nvfetcher configuration is a list of packages in TOML. You can find an example of the configuration file, see [`nvfetcher_example.toml`](nvfetcher_example.toml).

## Haskell library

nvfetcher itsetlf is a Haskell library as well, whereas the CLI program is just a trivial wrapper of the library. You can create a Haskell program depending on it directly, creating an entry point. In this case, we can define packages in Haskell language, getting rid of TOML constraints. Let's create a Haskell script `Main.hs`:

```haskell
import Development.NvFetcher

main :: IO ()
main = defaultMain defaultArgs packageSet

packageSet :: PackageSet ()
packageSet = do
  pypiPackage "feeluown-core" "feeluown"
  gitHubPackage "qliveplayer" ("IsoaSFlus", "QLivePlayer")
```

`runghc Main.hs build` will create `sources.nix` identical to the initial one. 
As you can see, packages are declared in `PackageSet ()`, an embedded DSL specialize in our case.

### Define a package

```haskell
data Package = Package
  { pname :: PackageName,
    pversion :: VersionSource,
    pfetcher :: Version -> NixFetcher Fresh
  }

data VersionSource
  = GitHubRelease {owner :: Text, repo :: Text}
  | Git {vurl :: Text}
  | Pypi {pypi :: Text}
  | ArchLinux {archpkg :: Text}
  | Aur {aur :: Text}
  | Manual {manual :: Text}

data NixFetcher (k :: Prefetch)
  = FetchGit
      { furl :: Text,
        rev :: Version,
        branch :: Maybe Text,
        deepClone :: Bool,
        fetchSubmodules :: Bool,
        leaveDotGit :: Bool,
        sha256 :: MapPrefetch k
      }
  | FetchUrl {furl :: Text, sha256 :: MapPrefetch k}
```

Data types all you need to define a package:
* `PackageName`: a synonym of `Text`, name of the package
* `VersionSource`, wired to nvchecker, how to track package's version
* `NixFetcher`, wired to nix-prefetch-{git,url}, how to fetch the package given the version

### Package set

`PackageSet` is a free monad under the hood, whereas you only need to concern the following functions,
which are enough to create `PackageSet`:

```haskell
package ::
  -- | package name
  PackageName ->
  -- | version source
  VersionSource ->
  -- | fetcher
  (Version -> NixFetcher Fresh) ->
  PackageSet ()

pypiPackage ::
  -- | package name
  PackageName ->
  -- | pypi name
  Text ->
  PackageSet ()

gitHubPackage ::
  -- | package name
  PackageName ->
  -- | owner and repo
  (Text, Text) ->
  PackageSet ()
```

Since `PackageSet` is a monad, you can concatenation packages using do notaion, like the example shown above.
