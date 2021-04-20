# nvfetcher

nvfetcher is a build system built on top of [shake](https://www.shakebuild.com/),
integrating [nvchecker](https://github.com/lilydjwg/nvchecker) and [nix-prefetch](https://github.com/msteen/nix-prefetch). It's very simple -- the system produces only one artifact as the result of build. For example:

Given the following Haskell script `Main.hs`

```haskell
import Development.NvFetcher

main :: IO ()
main = defaultMain defaultArgs packageSet

packageSet :: PackageSet ()
packageSet = do
  pypiPackage "feeluown-core" "feeluown"
  let fuoPlugins = mapM_ $ \x -> pypiPackage ("feeluown-" <> x) ("fuo_" <> x)
  fuoPlugins ["kuwo", "local"]
```

`$ runghc Main.hs` will create `sources.nix` shown below:

```nix
# sources.nix
{ fetchFromGitHub, fetchurl }:
{
  feeluown-core = {
    pname = "feeluown-core";
    version = "3.7.5";
    src = fetchurl {
      sha256 = "JA/a76/vylkbj+qcMfk1RBHZm8yqtLUwxVf+L1UFSjU=";
      url = "mirror://pypi/f/feeluown/feeluown-3.7.5.tar.gz";
    };
  };
  feeluown-kuwo = {
    pname = "feeluown-kuwo";
    version = "0.1.2";
    src = fetchurl {
      sha256 = "M/RDVmok9WMK7EXZ1+icRwBr5cD+hFt5rvRjdf9RmA0=";
      url = "mirror://pypi/f/fuo_kuwo/fuo_kuwo-0.1.2.tar.gz";
    };
  };
  feeluown-local = {
    pname = "feeluown-local";
    version = "0.2.1";
    src = fetchurl {
      sha256 = "p5mkKOXDz4aW6N2hJNfnrH3OGxUlsRAN9Ax0jksT1EU=";
      url = "mirror://pypi/f/fuo_local/fuo_local-0.2.1.tar.gz";
    };
  };
}
```



As you can see, packages are declared in `PackageSet ()`, an embedded DSL specialize in our case.
Shake will help us handle necessary rebuilds -- we check versions of packages during each run, but only prefetch them when needed.

## Usage

nvfetcher is not a cli tools, but a Haskell library. In order to use it, you will need to create an entry point for it with defined `PackageSet`.

### Define a package

```haskell
data Package = Package
  { pname :: PackageName,
    pversion :: VersionSource,
    pfetcher :: Version -> NixFetcher Fresh
  }

data VersionSource
  = GitHub {owner :: Text, repo :: Text}
  | Pypi {pypi :: Text}
  | ArchLinux {archpkg :: Text}
  | Aur {aur :: Text}
  | Manual {manual :: Text}

data NixFetcher (k :: Prefetch)
  = FetchFromGitHub
      { gitHubOwner :: Text,
        gitHubRepo :: Text,
        gitHubRev :: Version,
        sha256 :: MapPrefetch k
      }
  | FetchUrl {url :: Text, sha256 :: MapPrefetch k}
```

Data types all you need to define a package:
* `PackageName`: a synonym of `Text`, name of the package
* `VersionSource`, wired to nvchecker, how to track package's version
* `NixFetcher`, wired to nix-prefetch, how to fetch the package given the version

nvfetcher consults nvchecker to get the latest version, then calls nix-prefetch to
get SHA256 of package's source, filling it to `NixFetcher`, and finally pretty-prints the filled fetcher to nix expression.

### Package set

`PackageSet` is a trivial DSL constructed by free monad, whereas you only need to concern the following functions,
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

Since `PackageSet` is a monad, you can concatenation packages using do notaion, like initial example.
