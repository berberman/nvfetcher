# nvfetcher

[![Hackage](https://img.shields.io/hackage/v/nvfetcher.svg?logo=haskell)](https://hackage.haskell.org/package/nvfetcher)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![nix](https://github.com/berberman/nvfetcher/actions/workflows/nix.yml/badge.svg)](https://github.com/berberman/nvfetcher/actions/workflows/nix.yml)

nvfetcher is a tool to automate packages updates in flakes repos. It's built on top of [shake](https://www.shakebuild.com/),
integrating [nvchecker](https://github.com/lilydjwg/nvchecker).
It's very simple -- most complicated works are done by nvchecker, nvfetcher just wires it with prefetch tools,
producing only one artifact as the result of build.
nvfetcher cli program accepts a TOML file as config, which defines a set of package sources to run.

## Overview

For example, given the following configuration file:

```toml
# nvfetcher.toml
[feeluown-core]
src.pypi = "feeluown"
fetch.pypi = "feeluown"

[qliveplayer]
src.github = "IsoaSFlus/QLivePlayer"
fetch.github = "IsoaSFlus/QLivePlayer"
git.fetchSubmodules = true
```

running `nvfetcher build` will create `sources.nix` like:

```nix
# sources.nix
{ fetchgit, fetchurl }:
{
  feeluown-core = {
    pname = "feeluown-core";
    version = "3.7.7";
    src = fetchurl {
      sha256 = "06d3j39ff9znqxkhp9ly81lcgajkhg30hyqxy2809yn23xixg3x2";
      url = "https://pypi.io/packages/source/f/feeluown/feeluown-3.7.7.tar.gz";
    };
  };
  qliveplayer = {
    pname = "qliveplayer";
    version = "3.22.1";
    src = fetchgit {
      url = "https://github.com/IsoaSFlus/QLivePlayer";
      rev = "3.22.1";
      fetchSubmodules = true;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "00zqg28q5xrbgql0kclgkhd15fc02qzsrvi0qg8lg3qf8a53v263";
    };
  };
}
```

We tell nvfetcher how to get the latest version number of packages and how to fetch their sources given version numbers,
and nvfetcher will help us keep their version and prefetched SHA256 sums up-to-date, stored in `sources.nix`.
Shake will help us handle necessary rebuilds -- we check versions of packages during each run, but only prefetch them when needed.

## Live examples

How to use the generated sources file? Here are some examples:

* My [flakes repo](https://github.com/berberman/flakes)

* Nick Cao's [flakes repo](https://gitlab.com/NickCao/flakes/-/tree/master/pkgs)

## Usage

Basically, there are two ways to use nvfetcher, where the difference is how we provide package sources definitions to it. 
No matter which way you use it in, CLI options are inherited from shake with two targets, typically used as:

* `nvfetcher build` - our main purpose, creating `sources.nix`
* `nvfetcher clean` - clean up cache and remove `sources.nix`

> nvfetcher uses `build` as the target if no specified

> You can specify `-j` to enable parallelism

### CLI

To run nvfetcher as a CLI program, you'll need to provide package sources defined in TOML.

Aavailable CLI options:
* `-c` (`--config`) - path to the TOML configuration file
* `-o` (`--output`) - path to the output nix file
* `-v` (`--version`) - print nvfetcher version
* `-l` (`--log`) - path to log file, where nvfetcher dumps the version changes 

Each *package* corresponds to a TOML table, whose name is encoded as table key;
there are two fields and four optional git prefetch configuration in each table:
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

* optional git prefetch configuration, which makes sense only when the fetcher equals to `fetch.github` or `fetch.git`.
They can exist simultanesouly.
  * `git.branch = branch_name` - branch to fetch
  * `git.deepClone` - a bool value to control deep clone
  * `git.fetchSubmodules` - a bool value to control fetching submodules
  * `git.leaveDotGit` - a bool value to control leaving dot git

You can find an example of the configuration file, see [`nvfetcher_example.toml`](nvfetcher_example.toml).

### Haskell library

nvfetcher itsetlf is a Haskell library as well, whereas the CLI program is just a trivial wrapper of the library. You can create a Haskell program depending on it directly, creating an entry point. In this case, we can define packages in Haskell language, getting rid of TOML constraints.

You can find an example of using nvfetcher in the library way, see [`Main_example.hs`](Main_example.hs).

## Documentation

For details of the library, documentation of released versions is available on [Hackage](https://hackage.haskell.org/package/nvfetcher),
and of master is on our [github pages](https://nvfetcher.berberman.space).

## Limitations

There is no way to check the equality over version sources and fetchers, so If you change either of them in a package,
you will need to rebuild everything, i.e. run `nvfetcher clean` to remove shake databsae, to make sure that
our build system works correctly. We could automate this process, for example,
calculate the hash of the configuration file and bump `shakeVersion` to trigger the rebuild.
However, this shouldn't happen frequently and we want to minimize the changes, so it's left for you to do manually.

> Adding or removing a package doesn't require such rebuild

## Contributing

Issues and PRs are always welcome. **\_(:з」∠)\_**
