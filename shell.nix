{ pkgs? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/719ac402b1c13bb14cc496812c67fd7f65c7bf68.tar.gz") {}}:

with pkgs;

let
  postgresql = postgresql_12;
  frontendDeps = [ nodejs-14_x ];
in
  mkShell {
    buildInputs = [ glibcLocales postgresql zlib direnv hlint stylish-haskell gmp parallel haskellPackages.apply-refact]
      ++ frontendDeps;
  }
