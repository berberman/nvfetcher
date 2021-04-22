{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in with pkgs; {
        defaultPackage = nvfetcher;
        devShell = with haskell.lib;
          (addBuildTools (haskellPackages.nvfetcher) [
            haskell-language-server
            cabal-install
            nvchecker
            nix-prefetch-git
          ]).envFunc { };
      }) // {
        overlay = self: super:
          let
            hpkgs = super.haskellPackages;
            # Added to haskellPackages, so we can use it as a haskell library in ghcWithPackages
            nvfetcher = hpkgs.callCabal2nix "nvfetcher" ./. { };
            # Exposed to top-level nixpkgs, as an nvfetcher executable 
            nvfetcher-bin = with super;
              lib.overrideDerivation
              (haskell.lib.justStaticExecutables nvfetcher) (drv: {
                nativeBuildInputs = drv.nativeBuildInputs ++ [ makeWrapper ];
                postInstall = ''
                  EXE=${lib.makeBinPath [ nvchecker nix-prefetch-git ]}
                  wrapProgram $out/bin/nvfetcher \
                    --prefix PATH : "$out/bin:$EXE"
                '';
              });
          in {
            haskellPackages = super.haskellPackages.override
              (old: { overrides = hself: hsuper: { inherit nvfetcher; }; });
            nvfetcher = nvfetcher-bin;
          };

      };
}
