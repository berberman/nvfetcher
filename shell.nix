{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {

  buildInputs = [
    pkgs.cabal-install
    pkgs.prefetch-npm-deps
    pkgs.fourmolu
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.nvchecker
    pkgs.nix-prefetch-git
    pkgs.nix-prefetch-docker

    pkgs.openssl
    pkgs.zlib
  ];

}
