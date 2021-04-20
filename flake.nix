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
        in with super;
        with haskell.lib; {
          inherit nvfetcher;
          nvfetcher-dev =
            addBuildTools nvfetcher [ haskell-language-server cabal-install ];
        };
      defaultPackage.x86_64-linux = nvfetcher;
      devShell.x86_64-linux = (nvfetcher-dev.envFunc { }).overrideAttrs
        (old: { buildInputs = old.buildInputs ++ [ nvchecker nix-prefetch ]; });
    };
}
