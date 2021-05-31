{ mkDerivation, aeson, base, binary, binary-instances, bytestring
, containers, data-default, extra, free, lib, microlens
, microlens-th, neat-interpolation, optparse-simple, shake, text
, tomland, transformers, unordered-containers, validation-selective
}:
mkDerivation {
  pname = "nvfetcher";
  version = "0.2.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary binary-instances bytestring containers
    data-default extra free microlens microlens-th neat-interpolation
    shake text tomland transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson base binary binary-instances bytestring containers
    data-default extra free microlens microlens-th neat-interpolation
    optparse-simple shake text tomland transformers
    unordered-containers validation-selective
  ];
  homepage = "https://github.com/berberman/nvfetcher";
  description = "Generate nix sources expr for the latest version of packages";
  license = lib.licenses.mit;
}
