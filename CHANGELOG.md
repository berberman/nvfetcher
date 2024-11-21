# Revision history for nvfetcher

## 0.7.0.0

* Quote package name passed to nvchecker
* Add `url.name` option to specify the file name in prefetch
* Clean build dir before build
* Support keep going on fetch failure
* Add `--commit-summary`
* Support sparseCheckout

## 0.6.2.0

* Rework config parsing with toml-reader

## 0.6.1.0

* Replace `nix-prefetch` with `nix-prefetch-git` and `nix-prefetch-url`

## 0.6.0.0

* Parse error output from nvchecker
* Add source and fetcher for Docker containers (Thanks to @amesgen)
* Add option fetch.force to always rerun the prefetch rule
* Add `fetchGitHubRelease'` to `PackageSet` 
* Add `GetGitCommitDate` rule
* Add `--keyfile` to pass [nvchecker keyfile](https://nvchecker.readthedocs.io/en/latest/usage.html#configuration-table)
* Remove IFD from extracing sources

## 0.5.0.0

There have been many significant changes since the last release.
**Starting from this version, nvfetcher no longer requires shake database for each project,
in other words, there is no need to commit the database in git or share it between machines.**
Also, a machine-readable `generated.json` will be generated for convenience.

### Migration

The option in TOML configuration `cargo_lock` (string) was changed to `cargo_locks` (list of strings), since now nvfetcher supports handling multi-cargo locks.

* Use `fetchFromGitHub` as the GitHub fetcher (introduces [`nix-prefetch`](https://github.com/msteen/nix-prefetch))
* Add [cmd](https://nvchecker.readthedocs.io/en/latest/usage.html#find-with-a-command) version source
* Support pinning a package
* Tweak src name to extract .vsx file properly
* Add option --filter to specify packages to update
* Fix missing `argActionAfterBuild`
* Add one shot nvchecker rule independent of package definition
* Don't cache generated nix exprs in shake database
* Add `fetchTarball`
* Produce parser readable `generated.json`
* Internalize shake database
* Validate config before decoding
* Extract `Config` from `Arg` and save it to shake extra
* Breakdown `Args` to provide a more concise API
* Support multi-cargo lock files
* Pretty print rules in command line

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
