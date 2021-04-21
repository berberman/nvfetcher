{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in with pkgs; {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          nvfetcher = hpkgs.callCabal2nix "nvfetcher" ./. { };
          nvfetcher-bin = with super;
            lib.overrideDerivation (haskell.lib.justStaticExecutables nvfetcher)
            (drv: {
              nativeBuildInputs = drv.nativeBuildInputs ++ [ makeWrapper ];
              postInstall = ''
                EXE=${lib.makeBinPath [ nvchecker nix-prefetch-git ]}
                wrapProgram $out/bin/nvfetcher \
                  --prefix PATH : "$out/bin:$EXE"
              '';
            });
        in with super;
        with haskell.lib; {
          inherit nvfetcher nvfetcher-bin;
          nvfetcher-dev = addBuildTools nvfetcher [
            haskell-language-server
            cabal-install
            nvchecker
            nix-prefetch-git
          ];
        };
      defaultPackage.x86_64-linux = nvfetcher-bin;
      devShell.x86_64-linux = nvfetcher-dev.envFunc { };

    };
}
