# nvfetcher

[![Hackage](https://img.shields.io/hackage/v/nvfetcher.svg?logo=haskell)](https://hackage.haskell.org/package/nvfetcher)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![nix](https://github.com/berberman/nvfetcher/actions/workflows/nix.yml/badge.svg)](https://github.com/berberman/nvfetcher/actions/workflows/nix.yml)

nvfetcher is a tool to automate packages updates in flakes repos. It's built on top of [shake](https://www.shakebuild.com/),
integrating [nvchecker](https://github.com/lilydjwg/nvchecker).
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


`sources.nix` is a symlink of `_sources/generated.nix`, which means that the "build" results produced by `nvfetcher` are under `_sources`,
so you have to keep this directory for further use.

### Live examples

How to use the generated sources file? Here are some examples:

* My [flakes repo](https://github.com/berberman/flakes)

* Nick Cao's [flakes repo](https://gitlab.com/NickCao/flakes/-/tree/master/pkgs)

## Installation

`nvfetcher` package is available in [nixpkgs](https://github.com/NixOS/nixpkgs), so you can try it with:

```
$ nix-shell -p nvfetcher
```

This repo also has flakes support:

```
$ nix run github:berberman/nvfetcher
```

To use it as a Haskell library, the package is available on [Hackage](https://hackage.haskell.org/package/nvfetcher).
If you want to use the Haskell library from flakes, there is also a shell `ghcWithNvfetcher`:

```
$ nix develop github:berberman/nvfetcher#ghcWithNvfetcher
$ runghc Main.hs
```

where you can define packages in `Main.hs`. See [Haskell library](#Haskell-library) for details.

## Usage

Basically, there are two ways to use nvfetcher, where the difference is how we provide package sources definitions to it.

### CLI

To run nvfetcher as a CLI program, you'll need to provide package sources defined in TOML.

```
Available options:
  --version                Show version
  --help                   Show this help text
  -o,--output FILE         Path to output nix file (default: "sources.nix")
  --commit-changes         `git commit` changes in this run (with shake db)
  -l,--changelog FILE      Dump version changes to a file
  -j NUM                   Number of threads (0: detected number of processors)
                           (default: 0)
  -r,--retry NUM           Times to retry of some rules (nvchecker, prefetch,
                           nix-instantiate, etc.) (default: 3)
  -t,--timing              Show build time
  -v,--verbose             Verbose mode
  --no-output              Don't symlink generated.nix to the output path
  TARGET                   Two targets are available: 1.build 2.clean
                           (default: "build")
  -c,--config FILE         Path to nvfetcher TOML config
                           (default: "nvfetcher.toml")
```

Each *package* corresponds to a TOML table, whose name is encoded as table key, with
two required fields and three optional fields in each table.
You can find an example of the configuration file, see [`nvfetcher_example.toml`](nvfetcher_example.toml).

#### Nvchecker

Version source -- how do we track upstream version updates?
* `src.github = owner/repo` - the latest github release
* `src.github_tag = owner/repo` - the max github tag, usually used with list options (see below)
* `src.pypi = pypi_name` - the latest pypi release
* `src.git = git_url` (and an optional `src.branch = git_branch`) - **the latest commit** of a repo
* `src.archpkg = archlinux_pkg_name` -- the latest version of an archlinux package
* `src.aur = aur_pkg_name` -- the latest version of an aur package
* `src.manual = v` -- a fixed version, which never updates
* `src.repology = project:repo` -- the latest version from repology
* `src.webpage = web_url` and `src.regex` -- a string in webpage that matches with regex
* `src.httpheader = request_url` and `src.regex` -- a string in http header that matches with regex
* `src.openvsx = publisher.ext_name` -- the latest version of a vscode extension from open vsx
* `src.vsmarketplace = publisher.ext_name` -- the latest version of a vscode extension from vscode marketplace


Optional list options for some version sources (`src.github_tag`, `src.webpage`, and `src.httpheader`),
see the corresponding [nvchecker documentation](https://nvchecker.readthedocs.io/en/latest/usage.html#list-options) for details.
* `src.include_regex`
* `src.exclude_regex`
* `src.sort_version_key`
* `src.ignored`


Optional global options for all kinds of version sources,
see the corresponding [nvchecker documentation](https://nvchecker.readthedocs.io/en/latest/usage.html#global-options) for details. You can tweak obtained version number using this option, e.g. stripping the prefix `v` or transforming the result by regex.
* `src.prefix`
* `src.from_pattern`
* `src.to_pattern`

#### Nix fetcher

How do we fetch the package source if we have the target version number?
`$ver` is available in string, which will be set to the result of nvchecker.

* `fetch.github = owner/repo`
* `fetch.pypi = pypi_name`
* `fetch.git = git_url`
* `fetch.url = url`
* `fetch.openvsx = publisher.ext_name`
* `fetch.vsmarketplace = publisher.ext_name`


Optional `nix-prefetch-git` config, which make sense only when the fetcher equals to `fetch.github` or `fetch.git`.
They can exist simultanesouly.
  * `git.deepClone`
  * `git.fetchSubmodules`
  * `git.leaveDotGit`


#### Extract src

Optional *extract src* config, files are extracted into build directory, and then read by `readFile` in generated nix expr.
  * `extract = [ "file_1", "file_2", ...]` - file paths are relative to the source root

#### Rust support

`rustPlatform.buildRustPackage` now accepts an attribute [`cargoLock`](https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#importing-a-cargolock-file) to vendor dependencies from `Cargo.lock`,
so we can use this instead TOFU `cargoSha256` for Rust packageing. `nvfetcher` supports automating this process,
extracting the lock file to build and calculating `cargoLock.outputHashes`, as long as you set the config
* `cargo_lock = cargo_lock_path` - relative to the source root

#### Passthru

*passthru* config, an additional set of attrs to be generated.

  * `passthru = { k1 = "v1", k2 = "v2", ... }`

### Haskell library

nvfetcher itsetlf is a Haskell library as well, whereas the CLI program is just a trivial wrapper of the library.
You can create a Haskell program depending on it directly, by using the `runNvFetcher` entry point.
In this case, we can define packages in Haskell language, getting rid of TOML constraints.

You can find an example of using nvfetcher in the library way, see [`Main_example.hs`](Main_example.hs).

## Documentation

For details of the library, documentation of released versions is available on [Hackage](https://hackage.haskell.org/package/nvfetcher),
and of master is on our [github pages](https://nvfetcher.berberman.space).

## Contributing

Issues and PRs are always welcome. **\_(:з」∠)\_**

Building from source:

```
$ git clone https://github.com/berberman/nvfetcher
$ nix develop
$ cabal build
```
