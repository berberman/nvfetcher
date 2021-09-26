# Revision history for nvfetcher

## 0.4.0.0

* Rename `_build` to `_sources`
* Remove the symlink `sources.nix -> _sources/generated.nix`
  * Remove CLI option `--output` (was used to set the symlink source name, `sources.nix` by default)
  * Add CLI option `build-dir` to specify build directory (`_sources` by default)
* Add CLI option `--commit-changes` to commit changes of build directory
* Support openvsx and vsmarketplace version sources (needs new version of nvchecker)
* Support attributes pass through
* Fix the bug that Core rule was cut off even if the configuration has changed
(no longer needs to use `nvfetcher clean` to keep the build system consistency manually)
* Fix the parser of git source in Cargo.lock
* Fix wrong trailing white spances in generated nix expr
* Fix missing semicolon in generated nix expr that reads Cargo.lock file
* Enhance eDSL experience
* Add some unit tests

## 0.3.0.0

There are massive enhancements since the last release:

* Add support for nvchecker [list options](https://nvchecker.readthedocs.io/en/latest/usage.html#list-options)
* Refactor TOML config parsing
* Remove version specification in fetcher config (`fetch.url = url:version` -> `fetch.url = url`)
* Add support for calculating [`cargoLock`](https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#importing-a-cargolock-file) for `rustPlatform.buildRustPackage`
* Add support for nvchecker [global options](https://nvchecker.readthedocs.io/en/latest/usage.html#global-options)
* Remove ambiguous branch specification (`git.branch`) from git fetcher
* Enable parallelism by default
* Add a global retry option
* Rename `.shake` to `_build`
* Generate nix output file in `_build`, and symlink it to `../sources.nix` (You have to keep `_build` as the `nvfetcher` run result)
* Support extracting arbitrary files from fetched package source
* Add nvchecker upstream sources [`src.webpage`](https://nvchecker.readthedocs.io/en/latest/usage.html#search-in-a-webpage) and [`src.httpheader`](https://nvchecker.readthedocs.io/en/latest/usage.html#search-in-an-http-header)
* Add nvchecker upstream source `src.github_tag`
* Share CLI between `runNvfetcher` (use `nvfetcher` in the DSL way) and `nvfetcher` executable program 
* Nix related improvements:
  * Add a development shell `ghcWithNvfetcher` for people who want to use `nvfetcher` as a Haskell library
  * Generate command line completion for the executable
   

## 0.2.0.0

* Generated package sources will be sorted alphabetically.
* CLI program now supports `nix-git-prefetch` configurations in TOML.
* Lenses are added for some data types.
* CLI options are no loger inherited from Shake. Now `nvfetcher` has its own CLI options with completion support.

## 0.1.0.0

* First version. Released on an unsuspecting world.
