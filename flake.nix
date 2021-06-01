{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, flake-compat, ... }:
    with flake-utils.lib;
    eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in with pkgs; rec {
        defaultPackage = nvfetcher-bin;
        devShell = with haskell.lib;
          (addBuildTools (haskellPackages.nvfetcher) [
            haskell-language-server
            cabal-install
            nvchecker
            nix-prefetch-git
            cabal2nix # cd nix && cabal2nix ../. > default.nix && ..
          ]).envFunc { };
        packages.nvfetcher-lib = with haskell.lib;
          overrideCabal (haskellPackages.nvfetcher) (drv: {
            haddockFlags = [
              "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'"
            ];
          });
        packages.ghcWithNvfetcher = mkShell {
          buildInputs = [
            nix-prefetch-git
            nvchecker
            (haskellPackages.ghcWithPackages (p: [ p.nvfetcher ]))
          ];
        };
        hydraJobs = { inherit packages; };
      }) // {
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = hself: hsuper: {
              nvfetcher = with final.haskell.lib;
                generateOptparseApplicativeCompletion "nvfetcher"
                (overrideCabal (prev.haskellPackages.callPackage ./nix { })
                  (drv: {
                    buildTools = drv.buildTools or [ ] ++ [ final.makeWrapper ];
                    postInstall = with final;
                      drv.postInstall or "" + ''
                        wrapProgram $out/bin/nvfetcher \
                          --prefix PATH ":" "${
                            lib.makeBinPath [ nvchecker nix-prefetch-git ]
                          }"
                      '';
                  }));
            };
          });
          nvfetcher-bin = with final;
            haskell.lib.justStaticExecutables haskellPackages.nvfetcher;
        };
      };
}
