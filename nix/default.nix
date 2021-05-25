{ mkDerivation, aeson, base, binary, bytestring, containers, extra, free
, microlens, microlens-th, neat-interpolation, shake, stdenv, text, tomland
, transformers, validation-selective }:
mkDerivation {
  pname = "nvfetcher";
  version = "0.1.0.0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    binary
    bytestring
    containers
    extra
    free
    microlens
    microlens-th
    neat-interpolation
    shake
    text
    transformers
  ];
  executableHaskellDepends = [
    aeson
    base
    binary
    bytestring
    containers
    extra
    free
    microlens
    microlens-th
    neat-interpolation
    shake
    text
    tomland
    transformers
    validation-selective
  ];
  homepage = "https://github.com/berberman/nvfetcher";
  description = "Generate nix sources expr for the latest version of packages";
  license = stdenv.lib.licenses.mit;
}
