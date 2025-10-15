{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      flake-compat,
      ...
    }:
    with flake-utils.lib;
    eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
          config = {
            allowBroken = true;
          };
        };
      in
      with pkgs;
      rec {
        packages.default = nvfetcher-bin;
        devShells.default =
          with haskell.lib;
          (addBuildTools (haskellPackages.nvfetcher) [
            haskell-language-server
            cabal-install
            nvchecker
            nix-prefetch-git
            nix-prefetch-docker
            cabal2nix # cd nix && cabal2nix ../. > default.nix && ..
          ]).envFunc
            { };
        packages.nvfetcher-lib =
          with haskell.lib;
          overrideCabal (haskellPackages.nvfetcher) (drv: {
            haddockFlags = [
              "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'"
            ];
          });
        packages.ghcWithNvfetcher = mkShell {
          buildInputs = [
            nix-prefetch-git
            nix-prefetch-docker
            nvchecker
            (haskellPackages.ghcWithPackages (p: [ p.nvfetcher ]))
          ];
        };
        hydraJobs = { inherit packages; };
      }
    )
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
            hself: hsuper: {
              nvfetcher =
                with final.haskell.lib;
                hself.generateOptparseApplicativeCompletions [ "nvfetcher" ] (
                  overrideCabal (prev.haskellPackages.callPackage ./nix { }) (drv: {
                    # test needs network
                    # don't use `doCheck = false` here, because we still want to have test dependencies in dev shell
                    checkPhase = "";
                    buildTools = drv.buildTools or [ ] ++ [ final.makeWrapper ];
                    postInstall =
                      with final;
                      drv.postInstall or ""
                      + ''
                        wrapProgram $out/bin/nvfetcher \
                          --prefix PATH ":" "${
                            lib.makeBinPath [
                              python313Packages.awesomeversion
                              nvchecker
                              nix-prefetch-git
                              nix-prefetch-docker
                            ]
                          }"
                      '';
                  })
                );
            }
          );
        });
        nvfetcher-bin =
          with final;
          let
            hl = haskell.lib;
            nvfetcherStatic = hl.justStaticExecutables haskellPackages.nvfetcher;
            scope = nvfetcherStatic.scope;
          in
          hl.overrideCabal nvfetcherStatic (drv: {
            postInstall = ''
              ${drv.postInstall or ""}
              remove-references-to -t ${scope.shake} "$out/bin/.nvfetcher-wrapped"
              remove-references-to -t ${scope.js-jquery} "$out/bin/.nvfetcher-wrapped"
              remove-references-to -t ${scope.js-flot} "$out/bin/.nvfetcher-wrapped"
              remove-references-to -t ${scope.js-dgtable} "$out/bin/.nvfetcher-wrapped"
            '';
          });
      };
    };
}
